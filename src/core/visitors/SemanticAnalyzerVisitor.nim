import ../[astnodes, tokens, types, errors, parser]
import ../../utils/semanticAnalyzerUtils
import visitor
import std/[logging, tables, strutils, sequtils, os]

type
  ModuleError* = ref object of CatchableError

  Symbol = object
    token: Token
    symbolType: Type
    pub: bool

  Scope = ref object
    depth: Natural = 0
    symbolTable: Table[string, Symbol]
    case isGlobal: bool
    of false: parent: Scope
    of true: discard

  SemanticAnalyzerVisitor* = ref object of Visitor
    stdLibPath*: string

    currentScope: Scope
    symbolScopeStack: Table[string, seq[Scope]]

    expectedContextType: Type
    loopLevel: Natural = 0
    funcStack: seq[FuncStatement]

var logger = newConsoleLogger(fmtStr = "KOVYL [SemanticAnalyzer] $levelname: ")

proc semanticAnalyzerLogging*(enabled: bool) =
  if enabled:
    logger.levelThreshold = lvlAll
    addHandler(logger)
  else:
    logger.levelThreshold = lvlNone

proc newSemanticAnalyzerVisitor*(stdLibPath: string): SemanticAnalyzerVisitor =
  result = SemanticAnalyzerVisitor(stdLibPath: stdLibPath, expectedContextType: getUndefinedType(),
    currentScope: Scope(isGlobal: true, symbolTable: initTable[string, Symbol]()))
  info("SemanticAnalyzerVisitor initialized")

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) {.base.}

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) {.base.}

proc visitExpecting(self: SemanticAnalyzerVisitor, expr: Expression, expected: Type) =
  let context = self.expectedContextType
  self.expectedContextType = expected
  info("Expected context type is set as: ", $expected)
  self.visitExpression(expr)
  self.expectedContextType = context
  info("Expected context type revert to: ", $context)

proc coerce(self: SemanticAnalyzerVisitor, left: Expression, right: Expression, expected: Type): bool =
  info("attempt to coerce types")
  self.visitExpecting(left, expected)
  var arrays = false

  if left.returnType.eq getVecType(getCharType()):
    self.visitExpecting(right, getArrayType(left.returnType.vecBase, 0))
    arrays = true
  else:
    self.visitExpecting(right, left.returnType)

  if left.returnType.neq right.returnType:
    if right.returnType.eq getVecType(getCharType()):
      self.visitExpecting(left, getArrayType(right.returnType.vecBase, 0))
      arrays = true
    else:
      self.visitExpecting(left, right.returnType)

  return left.returnType.eq(right.returnType) or arrays

proc newSymbol(self: SemanticAnalyzerVisitor, name: Token, symbolType: Type, pub: bool) =
  self.currentScope.symbolTable[name.lexeme] = Symbol(token: name, symbolType: symbolType, pub: pub)
  self.symbolScopeStack.mgetOrPut(name.lexeme, @[]).add(self.currentScope)
  info((if pub: "Public s" else: "S") & "ymbol created: ", name.lexeme, " of type ", $symbolType, 
    " at the depth of the scope: ", $self.currentScope.depth)

proc pushScope(self: SemanticAnalyzerVisitor) =
  let depth = self.currentScope.depth
  self.currentScope = Scope(isGlobal: false, symbolTable: initTable[string, Symbol](), 
    parent: self.currentScope, depth: depth + 1)
  info("Scope pushed (", depth, " -> ", self.currentScope.depth, ")")

proc popScope(self: SemanticAnalyzerVisitor) =
  let scope = self.currentScope
  for name in scope.symbolTable.keys:
    discard self.symbolScopeStack[name].pop()
    info("Symbol removed: ", name)
  self.currentScope = scope.parent
  info("Scope popped (", scope.depth, " -> ", self.currentScope.depth, ")")

proc getSymbol(self: SemanticAnalyzerVisitor, name: string): Symbol =
  let scope = self.symbolScopeStack[name][^1]
  result = scope.symbolTable[name]
  info("got symbol: " & result.token.lexeme & " of type " & $result.symbolType, 
    " at the depth of the scope: ", $scope.depth, " (current: ", $self.currentScope.depth, ")")

proc symbolExists(self: SemanticAnalyzerVisitor, name: string): bool =
  let exists = name in self.symbolScopeStack and self.symbolScopeStack[name].len > 0
  info("checking symbol existence: " & name & " -> " & $exists)
  return exists

proc symbolExistsInCurrentScope(self: SemanticAnalyzerVisitor, name: string): bool =
  let exists = name in self.currentScope.symbolTable
  info("checking symbol existence in current scope: " & name & " -> " & $exists)
  return exists

proc overload(self: SemanticAnalyzerVisitor, name: string, overloadType: Type) =
  let scope = self.symbolScopeStack[name][^1]
  scope.symbolTable[name].symbolType.overloads[name & $overloadType] = overloadType
  info("Function ", name, " overloaded as ", overloadType)

# EXPRESSIONS

method visitNumberExpression*(visitor: SemanticAnalyzerVisitor, node: NumberExpression): auto =
  info("visiting NumberExpression")
  node.setType(inferNumberType(node, visitor.expectedContextType))
  info("exiting NumberExpression")

method visitNulExpression*(visitor: SemanticAnalyzerVisitor, node: NulExpression): auto =
  info("visiting NulExpression")

  if visitor.expectedContextType.kind in {typePtr, typeVec}:
    node.setType(visitor.expectedContextType)
  else:
    warn("Nul in non-pointer context")

  info("exiting NulExpression")

method visitBinaryExpression*(visitor: SemanticAnalyzerVisitor, node: BinaryExpression): auto =
  info("visiting BinaryExpression")

  let left = node.left
  let right = node.right

  if not visitor.coerce(left, right, visitor.expectedContextType):
    warn("coercion failed")
    node.newBinaryTypeMismatchError()

  elif node.trySetNumber():              discard
  elif node.trySetChar():                discard
  elif node.checkEqNeq(typeChar):        node.setType(getBoolType())
  elif node.checkEqNeq(typeArray):       node.setType(getBoolType())
  elif node.checkEqNeq(typeVec):         node.setType(getBoolType())
  elif node.checkEqNeq(typePtr):         node.setType(getBoolType())
  elif node.checkEqNeq(typeTuple):       node.setType(getBoolType())
  elif node.checkAndOr():                node.setType(getBoolType())
  elif node.checkEqNeqStrings():         node.setType(getBoolType())
  else:                                  node.newBinaryTypeMismatchError()

  info("exiting BinaryExpression")

method visitUnaryExpression*(visitor: SemanticAnalyzerVisitor, node: UnaryExpression): auto =
  info("visiting UnaryExpression")

  visitor.visitExpression(node.value)

  if   node.checkPlusMinus(): node.setType(node.value.returnType)
  elif node.checkNot():       node.setType(getBoolType())
  else:                       node.newUnaryTypeMismatchError()

  info("exiting UnaryExpression")

method visitIdentifierExpression*(visitor: SemanticAnalyzerVisitor, node: IdentifierExpression): auto =
  info("visiting IdentifierExpression")
  var error = false

  if not visitor.symbolExists(node.token.lexeme):
    newError(errUndeclaredSymbol, node.token, @{"@0": node.token.lexeme})
    error = true

  if not error:
    node.setType(visitor.getSymbol(node.token.lexeme).symbolType)

  info("exiting IdentifierExpression")

method visitCastExpression*(visitor: SemanticAnalyzerVisitor, node: CastExpression): auto =
  info("visiting CastExpression")

  let valueType = node.value.returnType
  let to = node.returnType

  let illegal = valueType.kind in {typePtr, typeVec, typeBool} or to.kind in {typePtr, typeVec, typeBool} 

  info("Type conversion attempt (", valueType, " -> ", to, ") -> ", not illegal)

  if illegal:
    newError(errCannotCast, node.token, @{"@0": $valueType, "@1": $to})
    node.returnType = getUndefinedType()

  info("exiting CastExpression")

method visitDerefExpression*(visitor: SemanticAnalyzerVisitor, node: DerefExpression): auto =
  info("visiting DerefExpression")
  var error = false

  visitor.visitExpecting(node.value, getPtrType(visitor.expectedContextType))
  if node.value.returnType.kind.neq typePtr:
    newError(errTypeMismatch, node.token, @{"@0": $typePtr, "@1": $node.value.returnType})
    error = true

  if not error:
    node.setType(node.value.returnType.ptrBase)

  info("exiting DerefExpression")

method visitArrayExpression*(visitor: SemanticAnalyzerVisitor, node: ArrayExpression): auto =
  info("visiting ArrayExpression")

  var expected = getUndefinedType()
  var derived = getUndefinedType()
  var error = false

  for expr in node.values:
    visitor.visitExpecting(expr, derived)
    if not expr.returnType.eq(getUndefinedType()) and not expr.returnType.eq(getNulType()):
      derived = expr.returnType
      break
  info("type was derived from the array elements as ", derived)

  if visitor.expectedContextType.eq typeArray:
    expected = visitor.expectedContextType.arrBase
  else:
    info("non-array context")

  if expected.eq(typeArray) and derived.eq(typeArray):
    if expected.length > derived.length:
      derived = expected
  if expected != derived:
    expected = derived

  info("visiting ArrayExpression values...")
  for expr in node.values:
    visitor.visitExpecting(expr, expected)

    if expr.returnType.eq(typeArray) and expected.eq(typeArray):
      if expr.returnType.length > expected.length:
        newError(errSize, expr.token, @{"@0": $expr.returnType, "@1": $expected})
        error = true
        break
      expr.returnType = getArrayType(expr.returnType.arrBase, expected.length)
      info("The size of the static array '" & expr.token.lexeme & "' has been determined to " & $expected.length)

    elif expected.neq(getUndefinedType()) and expr.returnType.neq expected:
      newError(errTypeMismatch, expr.token, @{"@0": $expected, "@1": $expr.returnType})
      error = true
      break

  if not error:
    node.setType(getArrayType(expected, node.values.len))

  info("exiting ArrayExpression")

method visitIndexExpression*(visitor: SemanticAnalyzerVisitor, node: IndexExpression): auto =
  info("visiting IndexExpression")
  var error = false

  visitor.visitExpecting(node.value, getArrayType(visitor.expectedContextType, 0))
  visitor.visitExpression(node.index)

  if node.value.returnType.kind notin {typeVec, typeArray}:
    newError(errTypeMismatch, node.token, @{"@0": $typeVec & " or " & $typeArray, 
      "@1": $node.value.returnType})
    error = true

  if not node.index.returnType.isNumber:
    newError(errTypeMismatch, node.token, @{"@0": "number", "@1": $node.value.returnType})
    error = true

  if not error:
    node.setType(
      if node.value.returnType.kind.eq typeVec:   node.value.returnType.vecBase
      else:                                       node.value.returnType.arrBase
    )

  info("exiting IndexExpression")

method visitTupleExpression*(visitor: SemanticAnalyzerVisitor, node: TupleExpression): auto =
  info("visiting TupleExpression")
  var error = false

  if visitor.expectedContextType.neq typeTuple:
    warn("non-tuple context")
    var elements = node.elements
    var elementTypes = initOrderedTable[string, Type]()

    for nameToken, expr in elements.pairs:
      visitor.visitExpecting(expr, getUndefinedType())
      elementTypes[nameToken.lexeme] = expr.returnType

    node.setType(getTupleType(elementTypes))

  else:
    let expected = visitor.expectedContextType.elements
    let elements = node.elements

    for name, typ in expected:
      if name notin elements.keys.toSeq.mapIt(it.lexeme):
        newError(errMissingArgument, node.token, @{"@0": name})
        error = true

    for nameToken, expr in elements.pairs:
      if nameToken.lexeme notin expected:
        let err = if nameToken.kind == tkNumber: errUnexpectedArgument else: errUnexpectedNamedArgument
        newError(err, expr.token, @{"@0": nameToken.lexeme})
        error = true
        continue

      let typ = expected[nameToken.lexeme]
      visitor.visitExpecting(expr, typ)
      if expr.returnType.neq typ:
        newError(errTypeMismatch, expr.token, @{"@0": $typ, "@1": $expr.returnType})
        error = true
        continue

    if not error:
      node.setType(visitor.expectedContextType)

  info("exiting TupleExpression")

method visitFieldExpression*(visitor: SemanticAnalyzerVisitor, node: FieldExpression): auto =
  info("visiting FieldExpression")
  visitor.visitExpression(node.value)
  
  let returnType = node.value.returnType

  block analysis:
    var fields = initOrderedTable[string, Type]()

    case returnType.kind 
    of typeTuple: fields = returnType.elements 
    of typeModule: fields = returnType.symbols
    else: 
      warn("getting a field from a fieldless type")
      newError(errFieldless, node.token, @{"@0": $returnType})
      break analysis

    if node.token.lexeme notin fields:
      newError(errHasNoField, node.token, @{"@0": $returnType, "@1": node.token.lexeme})
      break analysis

    info("field ", node.token.lexeme, " is correct")
    node.setType(fields[node.token.lexeme])

  info("exiting FieldExpression")

method visitCallExpression*(visitor: SemanticAnalyzerVisitor, node: CallExpression): auto =
  info("visiting CallExpression")

  visitor.visitExpression(node.value)
  var error = false
  let varType = node.value.returnType

  block checkAll:
    if varType.neq typeFunc:
      newError(errTypeMismatch, node.token, @{"@0": $typeFunc, "@1": $node.value.returnType})
      error = true
      break checkAll

    block checkDefault:
      info("checking node == first definded function...")
      if node.arguments.len != varType.arguments.len:
        warn("node arguments len != first definded function arguments len")
        break checkDefault

      if visitor.expectedContextType.neq varType.returnType:
        warn("expected context type != first definded function return type")

      for i, expr in node.arguments:
        let index = $i
        var expected = node.value.returnType.arguments[index]

        visitor.visitExpecting(expr, expected)

        if expected.kind.eq(typeArray) and expr.returnType.kind.eq(typeArray):
          if expected.arrBase.neq expr.returnType.arrBase:
            break checkDefault
          if expected.length < expr.returnType.length:
            break checkDefault
          elif expected.length > expr.returnType.length:
            expr.returnType = expected
            info("Array size promoted from ", expr.returnType.length, " to ", expected.length)

        if expr.returnType != expected:
          warn("argument types != first definded function argument types")
          break checkDefault

      info("node is first definded function")
      node.setType(node.value.returnType.returnType)
      break checkAll

    info("node is not first definded function")
    info("Creating a function type from arguments and context...")

    var arguments: OrderedTable[string, Type]
    for i, expr in node.arguments:
      visitor.visitExpression(expr)
      arguments[$i] = expr.returnType

    let funcType = getFuncType(arguments, visitor.expectedContextType)
    info("created type: ", funcType)

    block checkOverloads:
      info("checking overloads...")
      if varType.overloads.len == 0:
        warn("no overload was found")
        break checkOverloads

      for _, overload in varType.overloads.pairs:
        info("checking overload ", overload, " equals ", funcType, "...")
        if funcType == overload:
          info("perfect overload hit found")
          node.setType(overload.returnType)
          node.value.setType(overload)
          break checkAll
        else:
          var argsMatch = true
          if funcType.arguments.len == overload.arguments.len:
            for key in funcType.arguments.keys:
              let argType = funcType.arguments[key]
              let overloadArgType = overload.arguments[key]
              
              if argType.kind == typeArray and overloadArgType.kind == typeArray:
                if argType.arrBase != overloadArgType.arrBase:
                  argsMatch = false
                  break
                if argType.length < overloadArgType.length:
                  continue
                elif argType.length > overloadArgType.length:
                  argsMatch = false
                  break
              elif argType != overloadArgType:
                argsMatch = false
                break
          else:
            argsMatch = false
          
          if argsMatch:
            info("overload found with compatible arguments")
            node.setType(overload.returnType)
            node.value.setType(overload)
            break checkAll

      warn("No matching overloads found for function ", varType.funcName)

    let funcName = node.value.token
    var avaiableOverloadFormatted = "- " & funcName.lexeme & $varType

    for name, _ in varType.overloads.pairs:
      avaiableOverloadFormatted &= "\n- " & name

    newError(errFuncResolution, funcName, @{"@0": funcName.lexeme, "@1": $funcType, "@2": avaiableOverloadFormatted})

  info("exiting CallExpression")

# STATEMENTS

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  info("visiting DeclarationStatement")

  var error = false

  var expected = node.symbolType

  visitor.visitExpecting(node.value, expected)

  var valueType = node.value.returnType

  if expected.kind.eq(typeArray) and valueType.kind.eq typeArray:
    if expected.length == 0 and valueType.length != 0:
      expected = getArrayType(expected.arrBase, valueType.length)
      info("The size of the static array '" & node.name.lexeme & "' has been determined to " & $valueType.length)
    elif expected.length == 0 and valueType.length == 0:
      newError(errEmptyStaticArray, node.value.token)
      error = true

    if expected.arrBase.neq valueType.arrBase:
      newError(errTypeMismatch, node.name, @{"@0": $expected, "@1": $valueType})
      error = true
    if expected.length < valueType.length:
      newError(errSize, node.value.token, @{"@0": $valueType, "@1": $expected})
      error = true
    elif expected.length > valueType.length:
      valueType = getArrayType(expected.arrBase, expected.length)
      info("The size of the static array '" & node.value.token.lexeme & "' has been determined to " & $valueType.length)

  elif expected.neq valueType:
    newError(errTypeMismatch, node.name, @{"@0": $expected, "@1": $valueType})
    error = true

  if visitor.symbolExistsInCurrentScope(node.name.lexeme):
    let existing = visitor.getSymbol(node.name.lexeme)
    newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": existing.token.file,
      "@2": $existing.token.line, "@3": $existing.token.column})
    error = true
  
  if not error:
    visitor.newSymbol(node.name, node.value.returnType, node.pub)

  info("exiting DeclarationStatement")

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  info("visiting BlockStatement")

  var isCodeUnreachable: bool
  var returnToken: Token

  for stmt in node.statements:
    if isCodeUnreachable:
      warn("unreachable code found")
      newError(errUnreachableCode, returnToken, @{"@0": $returnToken.file, "@1": $returnToken.line,
        "@2": $returnToken.column})
      break
    visitor.visitStatement(stmt)
    if stmt of ReturnStatement:
      isCodeUnreachable = true
      returnToken = ReturnStatement(stmt).token

  info("exiting BlockStatement")

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  info("visiting AssignmentStatement")

  visitor.visitExpression(node.left)
  visitor.visitExpecting(node.value, node.left.returnType)
  
  if node.left.returnType.kind.eq(typeArray) and node.value.returnType.kind.eq(typeArray):
    if node.left.returnType.length < node.value.returnType.length:
      newError(errSize, node.value.token, @{"@0": $node.value.returnType, "@1": $node.left.returnType})
    else:
      node.value.returnType = getArrayType(node.value.returnType.arrBase, node.left.returnType.length)
      info("The size of the static array '" & node.left.token.lexeme & 
        "' has been determined to " & $node.value.returnType.length)

  elif node.left.returnType.neq node.value.returnType:
    newError(errTypeMismatch, node.left.token, @{"@0": $node.left.returnType, "@1": $node.value.returnType})

  info("exiting AssignmentStatement")

method visitBranchingStatement*(visitor: SemanticAnalyzerVisitor, node: BranchingStatement): auto =
  info("visiting BranchingStatement")
  
  visitor.pushScope()
  visitor.visitExpecting(node.condition, getBoolType())
  if node.condition.returnType.neq(getBoolType()):
    newError(errTypeMismatch, node.condition.token, @{"@0": $getBoolType(), "@1": $node.condition.returnType})
  
  visitor.visitStatement(node.ifBlock)
  visitor.popScope()
  
  for el in node.elifBlocks:
    visitor.pushScope()
    visitor.visitExpecting(el.cond, getBoolType())
    if el.cond.returnType.neq(getBoolType()):
      newError(errTypeMismatch, el.cond.token, @{"@0": $getBoolType(), "@1": $el.cond.returnType})
    
    visitor.visitStatement(el.elifBlock)
    visitor.popScope()
  
  if node.elseBlock != nil:
    visitor.pushScope()
    visitor.visitStatement(node.elseBlock)
    visitor.popScope()
  
  info("exiting BranchingStatement")

method visitBreakStatement*(visitor: SemanticAnalyzerVisitor, node: BreakStatement): auto =
  info("visiting BreakStatement")

  info("Checking loop level -> ", visitor.loopLevel)
  if visitor.loopLevel == 0:
    newError(errForbiddenLocation, node.token)

  info("exiting BreakStatement")

method visitContinueStatement*(visitor: SemanticAnalyzerVisitor, node: ContinueStatement): auto =
  info("visiting ContinueStatement")

  info("Checking loop level -> ", visitor.loopLevel)
  if visitor.loopLevel == 0:
    newError(errForbiddenLocation, node.token)

  info("exiting ContinueStatement")

method visitWhileStatement*(visitor: SemanticAnalyzerVisitor, node: WhileStatement): auto =
  info("visiting WhileStatement")
  
  visitor.loopLevel.inc
  info("Incrementing loop level: ", visitor.loopLevel - 1, " -> ", visitor.loopLevel)
  visitor.pushScope()
  
  visitor.visitExpecting(node.condition, getBoolType())
  if node.condition.returnType.neq(getBoolType()):
    newError(errTypeMismatch, node.condition.token, @{"@0": $getBoolType(), "@1": $node.condition.returnType})
  
  visitor.visitStatement(node.whileBlock)
  
  visitor.popScope()
  visitor.loopLevel.dec
  info("Decrementing loop level: ", visitor.loopLevel + 1, " -> ", visitor.loopLevel)
  
  info("exiting WhileStatement")

method visitDefaultStatement*(visitor: SemanticAnalyzerVisitor, node: DefaultStatement): auto =
  info("visiting DefaultStatement")

  if visitor.symbolExistsInCurrentScope(node.name.lexeme):
    let existing = visitor.getSymbol(node.name.lexeme)
    newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": existing.token.file,
      "@2": $existing.token.line, "@3": $existing.token.column})

  elif node.symbolType.eq(typeArray) and node.symbolType.length == 0:
    newError(errEmptyStaticArray, node.name)

  elif node.symbolType.eq(typeFunc):
    newError(errFuncSignatureUnknown, node.name)

  else:
    visitor.newSymbol(node.name, node.symbolType, node.pub)

  info("exiting DefaultStatement")

proc blockEndsWithReturn(self: SemanticAnalyzerVisitor, node: Statement): bool =
  if node of ReturnStatement:
    return true
  elif node of BlockStatement:
    for stmt in BlockStatement(node).statements:
      if self.blockEndsWithReturn(stmt):
        return true
    return false
  elif node of BranchingStatement:
    let br = BranchingStatement(node)
    let ifEnds = self.blockEndsWithReturn(br.ifBlock)
    let elseEnds = br.elseBlock != nil and self.blockEndsWithReturn(br.elseBlock)
    
    var allElifsEnd = true
    for el in br.elifBlocks:
      if not self.blockEndsWithReturn(el.elifBlock):
        allElifsEnd = false
        break
    
    if br.elseBlock != nil and ifEnds and allElifsEnd and elseEnds:
      return true
    else:
      return false
  else:
    return false

method visitFuncStatement*(visitor: SemanticAnalyzerVisitor, node: FuncStatement): auto =
  info("visiting FuncStatement")

  var error = false

  if node.returnType.eq(typeArray) and node.returnType.length == 0:
    newError(errFuncEmptyStaticArray, node.name)
    error = true

  var argumentTypes: OrderedTable[string, Type]

  for argName, funcArg in node.arguments:
    if funcArg.expectedType.eq(typeArray) and funcArg.expectedType.length == 0:
      newError(errFuncEmptyStaticArray, node.name)
      error = true
    argumentTypes[argName] = funcArg.expectedType

  let funcType = getFuncType(argumentTypes, node.returnType, node.name.lexeme)

  if node.returnType.neq getUndefinedType():
    info("Checking that all paths in the function '", node.name.lexeme, "' block end with the return expression")
    if not visitor.blockEndsWithReturn(node.funcBlock):
      warn("...false")
      newError(errMissingReturn, node.name, @{"@0": node.name.lexeme})
      error = true
    else:
      info("...true")

  if not error:
    info("Checking function overloads...")
    if visitor.symbolExists(node.name.lexeme):
      var funcSymbol = visitor.getSymbol(node.name.lexeme)
      error = false

      if funcSymbol.symbolType.eq funcType:
        newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": funcSymbol.token.file,
            "@2": $funcSymbol.token.line, "@3": $funcSymbol.token.column})
        error = true

      for _, overType in funcSymbol.symbolType.overloads.pairs:
        if overType.eq funcType:
          newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": funcSymbol.token.file,
              "@2": $funcSymbol.token.line, "@3": $funcSymbol.token.column})
          error = true
          break

      if not error:
        visitor.overload(node.name.lexeme, 
          getFuncType(funcType.arguments, funcType.returnType, node.name.lexeme & $funcType))
        node.name.lexeme &= $funcType

    else:
      info("New function type is set as: ", funcType)
      node.funcType = funcType
      visitor.newSymbol(node.name, funcType, node.pub)

  if not error:
    visitor.pushScope()

    for _, funcArg in node.arguments:
      visitor.newSymbol(funcArg.origin, funcArg.expectedType, false)

    visitor.funcStack.add(node)
    visitor.visitStatement(node.funcBlock)
    discard visitor.funcStack.pop()

    visitor.popScope()

  info("exiting FuncStatement")

method visitReturnStatement*(visitor: SemanticAnalyzerVisitor, node: ReturnStatement): auto =
  info("visiting ReturnStatement")

  info("Checking func level -> ", visitor.funcStack.len)
  if visitor.funcStack.len == 0:
    newError(errForbiddenLocation, node.token)

  if (not node.hasValue) and visitor.funcStack[^1].returnType.neq getUndefinedType():
    newError(errExpression, node.token, @{"@0": "return without expression"})

  else:
    visitor.visitExpecting(node.value, visitor.funcStack[^1].returnType)
    if node.value.returnType.neq visitor.funcStack[^1].returnType:
      newError(errTypeMismatch, node.value.token, @{"@0": $visitor.funcStack[^1].returnType, 
        "@1": $node.value.returnType})

  info("exiting ReturnStatement")

method visitForStatement*(visitor: SemanticAnalyzerVisitor, node: ForStatement): auto =
  info("visiting ForStatement")

  visitor.pushScope()

  visitor.visitExpression(node.value)

  if node.value.returnType.kind notin {typeArray, typeVec}:
    newError(errTypeMismatch, node.token, @{"@0": $typeArray & " | " & $typeVec, "@1": $node.value.returnType})

  else:
    let varType = (if node.value.returnType.eq typeArray: node.value.returnType.arrBase
      else: node.value.returnType.vecBase)

    visitor.newSymbol(node.name, varType, false)

    visitor.loopLevel.inc
    visitor.visitStatement(node.forBlock)
    visitor.loopLevel.dec

  visitor.popScope()

  info("exiting ForStatement")

method visitCallStatement*(visitor: SemanticAnalyzerVisitor, node: CallStatement): auto =
  info("visiting CallStatement")

  visitor.visitExpression(node.callExpression)
  let funcExpr = node.callExpression

  if funcExpr.returnType.neq getUndefinedType():
    newError(errUnusedReturn, funcExpr.value.token, @{"@0": funcExpr.token.lexeme})

  info("exiting CallStatement")

method visitModuleStatement*(visitor: SemanticAnalyzerVisitor, node: ModuleStatement): auto =
  info("visiting ModuleStatement")

  block analysis:
    if visitor.symbolExistsInCurrentScope(node.name.lexeme):
      let existing = visitor.getSymbol(node.name.lexeme)
      newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": existing.token.file,
        "@2": $existing.token.line, "@3": $existing.token.column})
      break analysis

    let currentFile = node.name.file
    let modulePath = node.path.lexeme

    let (dir, _, _) = splitFile(currentFile)

    let fullPath = (
      if modulePath.startsWith("std/"): visitor.stdLibPath / modulePath[4..^1] & ".kvl"
      else: joinPath(dir, modulePath)
    )

    info("search for the ", node.path.lexeme, " module... (full path: ", fullPath, ")")

    if not fileExists(fullPath):
      warn("module ", node.path.lexeme, " does not exists")
      newError(errModuleNotFound, node.path, @{"@0": node.path.lexeme})
      break analysis

    info("found module ", node.path.lexeme, " on the path ", fullPath)

    let text = readFile(fullPath)

    var parser = newParser(text, fullPath)
    node.moduleBlock = parser.parse()

    if errors.errors.len != 0:
      newError(errCorruptedModule, node.name, @{"@0": node.path.lexeme})

    visitor.pushScope()
    info("semantic analysis of the ", node.path.lexeme, " module...")
    visitor.visitStatement(node.moduleBlock)

    info("creating a module type...")

    var symbols: OrderedTable[string, Type]
    for name, symbol in visitor.currentScope.symbolTable.pairs:
      if symbol.pub:
        info("Public symbol added to the module type: ", name)
        symbols[name] = symbol.symbolType
      else:
        info("Private symbol was skipped: ", name)

    visitor.popScope()


    if errors.errors.len != 0:
      newError(errCorruptedModule, node.name, @{"@0": node.path.lexeme})
    else:
      node.moduleType = getModuleType(fullPath, symbols)
      visitor.newSymbol(node.name, node.moduleType, false)

  info("exiting ModuleStatement")

method visitClosureStatement*(visitor: SemanticAnalyzerVisitor, node: ClosureStatement): auto =
  info("visiting ClosureStatement")

  info("checking function level -> ", visitor.funcStack.len)
  if visitor.funcStack.len == 0:
    newError(errForbiddenLocation, node.token)

  else:
    var error = false

    for name in node.names:
      info("closing symbol ", name.lexeme, "...")
      if not visitor.symbolExists(name.lexeme):
        newError(errUndeclaredSymbol, name, @{"@0": name.lexeme})
        error = true
      else:
        let symbol = visitor.getSymbol(name.lexeme)
        visitor.funcStack[^1].funcClosures.add(name.lexeme)
        info("symbol ", name.lexeme, " added to function ", visitor.funcStack[^1].name.lexeme, " closures")

        if symbol.symbolType.eq typeFunc:
          info("closing ", name.lexeme, " overloads...")
          for name, overload in symbol.symbolType.overloads.pairs:
            visitor.funcStack[^1].funcClosures.add(name)
            info("overload ", name, " added to function ", 
              visitor.funcStack[^1].name.lexeme, " closures")

  info("exiting ClosureStatement")

# SPECIALS

proc checkUnexpected(self: SpecialExpression | SpecialStatement, expected: seq[string]) =
  info("checking for unexpected arguments in special")

  for token, _ in self.namedArgs.pairs:
    let key = token.lexeme
    if key notin expected:
      if token.kind == tkIdentifier:
        warn("unexpected named argument found: ", key)
        newError(errUnexpectedNamedArgument, token, @{"@0": key})
      else:
        warn("unexpected argument found: ", key)
        newError(errUnexpectedArgument, token, @{"@0": key})

proc get(self: SpecialExpression | SpecialStatement, key: string): Expression =
  info("getting argument with key: ", key, "...")
  for token, expr in self.namedArgs.pairs:
    let k = if token.kind == tkNumber: token.lexeme else: token.lexeme
    if k == key:
      info("argument found for key: ", key)
      return expr
  warn("argument not found for key: ", key)
  newError(errMissingArgument, self.token, @{"@0": key})
  return newErrorExpression(self.token)

proc add(self: SpecialExpression | SpecialStatement, key: string, expr: Expression) =
  info("adding argument with key: ", key, " and expression type: ", $expr.returnType)
  let token = tkIdentifier.newToken(key, self.token.file, self.token.line, self.token.column, self.token.offset)
  self.namedArgs[token] = expr

proc has(self: SpecialExpression | SpecialStatement, key: string): bool =
  info("checking if argument exists with key: ", key, "...")
  for token, _ in self.namedArgs.pairs:
    let k = token.lexeme
    if k == key:
      info("argument exists with key: ", key)
      return true
  info("argument does not exist with key: ", key)
  return false

proc expect(self: SpecialExpression | SpecialStatement, key: string, types: varargs[Type]): bool =
  info("expecting argument with key: ", key, " and types: ", types.mapIt($it).join(" | "), "...")
  let expr = self.get(key)
  if expr of ErrorExpression:
    warn("argument is error expression")
    return false
  
  var matched = false
  for typ in types:
    if expr.returnType.neq typ:
      continue
    matched = true
    break
  
  if not matched:
    let expectedTypes = types.mapIt($it).join(" | ")
    warn("type mismatch for argument '", key, "': expected ", expectedTypes, ", got ", $expr.returnType)
    newError(errTypeMismatch, expr.token, @{"@0": expectedTypes, "@1": $expr.returnType})
    return false
  
  info("argument '", key, "' has correct type: ", $expr.returnType)
  return true

proc expect(self: SpecialExpression | SpecialStatement, key: string, types: varargs[TypeKind]): bool =
  info("expecting argument with key: ", key, " and types: ", types.mapIt($it).join(" | "), "...")
  let expr = self.get(key)
  if expr of ErrorExpression:
    warn("argument is error expression")
    return false
  
  var matched = false
  for typ in types:
    if expr.returnType.kind.neq typ:
      continue
    matched = true
    break
  
  if not matched:
    let expectedTypes = types.mapIt($it).join(" | ")
    warn("type mismatch for argument '", key, "': expected ", expectedTypes, ", got ", $expr.returnType)
    newError(errTypeMismatch, expr.token, @{"@0": expectedTypes, "@1": $expr.returnType})
    return false
  
  info("argument '", key, "' has correct type: ", $expr.returnType)
  return true

method visitSpecialExpression*(visitor: SemanticAnalyzerVisitor, node: SpecialExpression): auto =
  info("visiting SpecialExpression")

  block analysis:
    case node.kind:
    of skNew: 
      info("Semantic analysis of skNew special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      if visitor.expectedContextType.eq typePtr:
        visitor.visitExpecting(expr, visitor.expectedContextType.ptrBase)
      else:
        warn("non-ptr context")
        visitor.visitExpression(expr)

      node.setType(getPtrType(expr.returnType))

    of skVec: 
      info("Semantic analysis of skVec special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      var expected = getUndefinedType()
      if visitor.expectedContextType.kind.eq typeVec:
        expected = getArrayType(visitor.expectedContextType.vecBase, 0)
      else:
        warn("non-array context")

      visitor.visitExpecting(expr, expected)
      if not node.expect("0", typeArray): break analysis

      if expr of TypeExpression:
        node.add("@", newBoolExpression(expr.token.newFrom(kind = tkTrue)))

      node.setType(getVecType(expr.returnType.arrBase))

    of skLen:
      info("Semantic analysis of skLen special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      visitor.visitExpression(expr)
      if not node.expect("0", typeVec, typeArray): break analysis

      node.setType(getInt64Type())

    of skFmt:
      info("Semantic analysis of skFmt special")
      for key, expr in node.namedArgs.pairs:
        if key.kind == tkIdentifier and key.lexeme != "sep" and key.lexeme != "repr":
          warn("unexpected named argument found: ", key.lexeme)
          newError(errUnexpectedNamedArgument, key, @{"@0": key.lexeme})
          continue
        visitor.visitExpecting(expr, getArrayType(getCharType(), 0))
        if expr.returnType.eq(getArrayType(getCharType(), 0)):
          continue
        if expr.returnType.eq getVecType(getCharType()): 
          continue
        if expr.returnType.kind in {typeArray, typeVec, typePtr, typeNul, typeUndefined}:
          newError(errTypeMismatch, expr.token, @{"@0": "formatted type", "@1": $expr.returnType})

      if node.has("sep"):
        visitor.visitExpecting(node.get("sep"), getArrayType(getCharType(), 0))
        if not node.expect("sep", getArrayType(getCharType(), 0)): break analysis

      if node.has("repr"):
        visitor.visitExpecting(node.get("repr"), getBoolType())
        if not node.expect("repr", getBoolType()): break analysis

      node.setType(getVecType(getCharType()))

    of skTake:
      info("Semantic analysis of skTake special")
      if visitor.expectedContextType.kind.neq(typeArray):
        newError(errUnknownSize, node.token)
        break analysis
      elif visitor.expectedContextType.length == 0:
        newError(errEmptyStaticArray, node.token)
        break analysis

      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      visitor.visitExpecting(expr, getVecType(visitor.expectedContextType.arrBase))
      if not node.expect("0", typeVec): break analysis

      node.add("length", newNumberExpression(node.token.newFrom(kind = tkNumber,
        lexeme = $visitor.expectedContextType.length)))

      node.setType(getArrayType(expr.returnType.vecBase, visitor.expectedContextType.length))

    of skTakeof:
      info("Semantic analysis of skTakeof special")
      node.checkUnexpected(expected = @["0", "1"])
      let typ = node.get("0")

      visitor.visitExpression(typ)
      if not node.expect("0", typeArray): break analysis

      elif not (typ of TypeExpression):
        newError(errTypeMismatch, typ.token, @{"@0": "type annotation", "@1": "Expression"})
        break analysis

      elif typ.returnType.length == 0:
        newError(errEmptyStaticArray, node.token)
        break analysis

      let expr = node.get("1")

      visitor.visitExpecting(expr, getVecType(typ.returnType.arrBase))
      if not node.expect("1", typeVec): break analysis

      node.add("length", newNumberExpression(node.token.newFrom(kind = tkNumber,
        lexeme = $typ.returnType.length)))

      node.setType(getArrayType(typ.returnType.arrBase, typ.returnType.length))

    of skRead:
      info("Semantic analysis of skRead special")
      node.setType(getVecType(getCharType()))

    else:
      warn("Unhandled special expression: ", node.kind)

  info("exiting SpecialExpression")

method visitSpecialStatement*(visitor: SemanticAnalyzerVisitor, node: SpecialStatement): auto =
  info("visiting SpecialStatement")

  block analysis:
    case node.kind:
    of skPrint:
      info("Semantic analysis of skPrint special")
      node.checkUnexpected(expected = @["0", "term", "free"])
      let expr = node.get("0")

      visitor.visitExpecting(expr, getVecType(getCharType()))
      if not node.expect("0", getVecType(getCharType())): break analysis

      if node.has("term"):
        visitor.visitExpecting(node.get("term"), getArrayType(getCharType(), 0))
        if not node.expect("term", getArrayType(getCharType(), 0)): break analysis

      if node.has("free"):
        visitor.visitExpecting(node.get("free"), getBoolType())
        if not node.expect("free", getBoolType()): break analysis

    of skFree:
      info("Semantic analysis of skFree special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      visitor.visitExpression(expr)
      if not node.expect("0", typeVec, typePtr): break analysis

    of skAssert:
      info("Semantic analysis of skAssert special")
      node.checkUnexpected(expected = @["0", "1"])
      let cond = node.get("0")

      visitor.visitExpecting(cond, getBoolType())
      if not node.expect("0", getBoolType()): break analysis

      if node.has("1"):
        visitor.visitExpecting(node.get("1"), getArrayType(getCharType(), 0))
        if not node.expect("1", getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

    of skResize:
      info("Semantic analysis of skResize special")
      node.checkUnexpected(expected = @["0", "1"])
      let value = node.get("0")
      let size = node.get("1")
      
      visitor.visitExpression(value)
      if value.returnType.neq typeVec: 
        newError(errTypeMismatch, value.token, @{"@0": $typeVec, "@1": $value.returnType})
        break analysis

      visitor.visitExpression(size)
      if not node.expect("1", getInt64Type()): break analysis

    of skPanic:
      info("Semantic analysis of skPanic special")
      node.checkUnexpected(expected = @["0", "1"])
      let panicCode = node.get("0")
      let msg = node.get("1")

      visitor.visitExpecting(panicCode, getArrayType(getCharType(), 0))
      if not node.expect("0", getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

      visitor.visitExpecting(msg, getArrayType(getCharType(), 0))
      if not node.expect("1", getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

    else:
      warn("Unhandled special statement: ", node.kind)

  info("exiting SpecialStatement")

# GENERAL

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) =
  if node of ErrorExpression: discard
  elif node of BoolExpression: discard
  elif node of CharExpression: discard
  elif node of TypeExpression: discard
  elif node of NumberExpression: 
    visitor.visitNumberExpression(NumberExpression(node))
  elif node of NulExpression: 
    visitor.visitNulExpression(NulExpression(node))
  elif node of BinaryExpression:
    visitor.visitBinaryExpression(BinaryExpression(node))
  elif node of UnaryExpression:
    visitor.visitUnaryExpression(UnaryExpression(node))
  elif node of IdentifierExpression:
    visitor.visitIdentifierExpression(IdentifierExpression(node))
  elif node of CastExpression:
    visitor.visitCastExpression(CastExpression(node))
  elif node of DerefExpression:
    visitor.visitDerefExpression(DerefExpression(node))
  elif node of ArrayExpression:
    visitor.visitArrayExpression(ArrayExpression(node))
  elif node of IndexExpression:
    visitor.visitIndexExpression(IndexExpression(node))
  elif node of SpecialExpression:
    visitor.visitSpecialExpression(SpecialExpression(node))
  elif node of TupleExpression:
    visitor.visitTupleExpression(TupleExpression(node))
  elif node of FieldExpression:
    visitor.visitFieldExpression(FieldExpression(node))
  elif node of CallExpression:
    visitor.visitCallExpression(CallExpression(node))
  else:
    warn("unhandled expression")

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) =
  if node of ErrorStatement: discard
  elif node of DeclarationStatement:
    visitor.visitDeclarationStatement(DeclarationStatement(node))
  elif node of BlockStatement:
    visitor.visitBlockStatement(BlockStatement(node))
  elif node of AssignmentStatement:
    visitor.visitAssignmentStatement(AssignmentStatement(node))
  elif node of BranchingStatement:
    visitor.visitBranchingStatement(BranchingStatement(node))
  elif node of SpecialStatement:
    visitor.visitSpecialStatement(SpecialStatement(node))
  elif node of SpecialStatement:
    visitor.visitSpecialStatement(SpecialStatement(node))
  elif node of BreakStatement:
    visitor.visitBreakStatement(BreakStatement(node))
  elif node of ContinueStatement:
    visitor.visitContinueStatement(ContinueStatement(node))
  elif node of WhileStatement:
    visitor.visitWhileStatement(WhileStatement(node))
  elif node of DefaultStatement:
    visitor.visitDefaultStatement(DefaultStatement(node))
  elif node of FuncStatement:
    visitor.visitFuncStatement(FuncStatement(node))
  elif node of ReturnStatement:
    visitor.visitReturnStatement(ReturnStatement(node))
  elif node of ForStatement:
    visitor.visitForStatement(ForStatement(node))
  elif node of CallStatement:
    visitor.visitCallStatement(CallStatement(node))
  elif node of ModuleStatement:
    visitor.visitModuleStatement(ModuleStatement(node))
  elif node of ClosureStatement:
    visitor.visitClosureStatement(ClosureStatement(node))
  else:
    warn("unhandled statement")