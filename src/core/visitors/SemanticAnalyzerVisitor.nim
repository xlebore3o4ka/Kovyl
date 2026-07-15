import ../[astnodes, tokens, types, errors]
import ../../utils/semanticAnalyzerUtils
import visitor
import std/[logging, tables, strutils, sequtils]

type
  Symbol = object
    token: Token
    symbolType: Type

  Scope = ref object
    depth: Natural = 0
    symbolTable: Table[string, Symbol]
    case isGlobal: bool
    of false: parent: Scope
    of true: discard

  SemanticAnalyzerVisitor* = ref object of Visitor
    currentScope: Scope
    symbolScopeStack: Table[string, seq[Scope]]

    expectedContextType: Type
    loopLevel: Natural = 0

var logger = newConsoleLogger(fmtStr = "KOVYL [SemanticAnalyzer] $levelname: ")

proc semanticAnalyzerLogging*(enabled: bool) =
  if enabled:
    logger.levelThreshold = lvlAll
    addHandler(logger)
  else:
    logger.levelThreshold = lvlNone

proc newSemanticAnalyzerVisitor*(): SemanticAnalyzerVisitor =
  result = SemanticAnalyzerVisitor(expectedContextType: getUndefinedType(),
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
  self.visitExpecting(right, left.returnType)

  if left.returnType.neq right.returnType:
    self.visitExpecting(left, right.returnType)

  return left.returnType.eq right.returnType

proc newSymbol(self: SemanticAnalyzerVisitor, name: Token, symbolType: Type) =
  self.currentScope.symbolTable[name.lexeme] = Symbol(token: name, symbolType: symbolType)
  self.symbolScopeStack.mgetOrPut(name.lexeme, @[]).add(self.currentScope)
  info("Symbol created: ", name.lexeme, " of type ", $symbolType, 
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

# EXPRESSIONS

method visitNumberExpression*(visitor: SemanticAnalyzerVisitor, node: NumberExpression): auto =
  info("visiting NumberExpression")
  node.setType(inferNumberType(node, visitor.expectedContextType))
  info("exiting NumberExpression")

method visitNulExpression*(visitor: SemanticAnalyzerVisitor, node: NulExpression): auto =
  info("visiting NulExpression")

  if visitor.expectedContextType.kind in {typePtr, typeArray}:
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
  elif node.checkEqNeq(typeChar):        node.setType(getBoolType())
  elif node.checkEqNeq(typeStaticArray): node.setType(getBoolType())
  elif node.checkEqNeq(typeArray):       node.setType(getBoolType())
  elif node.checkEqNeq(typePtr):         node.setType(getBoolType())
  elif node.checkAndOr():                node.setType(getBoolType())
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

  let illegal = valueType.kind in {typePtr, typeArray, typeBool} or to.kind in {typePtr, typeArray, typeBool} 

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
  var error = false

  if visitor.expectedContextType.kind.eq typeStaticArray:
    expected = visitor.expectedContextType.staticArrBase
  else:
    warn("Non-static-array context")

  info("visiting ArrayExpression values...")
  for expr in node.values:
    visitor.visitExpecting(expr, expected)
    if expr.returnType.neq expected:
      newError(errTypeMismatch, expr.token, @{"@0": $expected, "@1": $expr.returnType})
      error = true
      break

  if not error:
    node.setType(getStaticArrayType(expected, node.values.len))

  info("exiting ArrayExpression")

method visitIndexExpression*(visitor: SemanticAnalyzerVisitor, node: IndexExpression): auto =
  info("visiting IndexExpression")
  var error = false

  visitor.visitExpecting(node.value, getStaticArrayType(visitor.expectedContextType, 0))
  visitor.visitExpression(node.index)

  if node.value.returnType.kind notin {typeArray, typeStaticArray}:
    newError(errTypeMismatch, node.token, @{"@0": $typeArray & " or " & $typeStaticArray, 
      "@1": $node.value.returnType})
    error = true

  if not node.index.returnType.isNumber:
    newError(errTypeMismatch, node.token, @{"@0": "number", "@1": $node.value.returnType})
    error = true

  if not error:
    node.setType(
      if node.value.returnType.kind.eq typeArray: node.value.returnType.arrBase
      else:                                       node.value.returnType.staticArrBase
    )

  info("exiting IndexExpression")

# STATEMENTS

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  info("visiting DeclarationStatement")

  var error = false

  let expected = node.symbolType

  visitor.visitExpecting(node.value, expected)

  let valueType = node.value.returnType

  if expected.kind.eq(typeStaticArray) and valueType.kind.eq typeStaticArray:
    if expected.length == 0 and valueType.length != 0:
      expected.length = valueType.length
      info("The size of the static array '" & node.name.lexeme & "' has been determined to " & $valueType.length)
    elif expected.length == 0 and valueType.length == 0:
      newError(errEmptyStaticArray, node.value.token)
      error = true

    if expected.staticArrBase.neq valueType.staticArrBase:
      newError(errTypeMismatch, node.name, @{"@0": $expected, "@1": $valueType})
      error = true
    if expected.length < valueType.length:
      newError(errSize, node.value.token, @{"@0": $valueType, "@1": $expected})
      error = true
    elif expected.length > valueType.length:
      valueType.length = expected.length
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
    visitor.newSymbol(node.name, node.value.returnType)

  info("exiting DeclarationStatement")

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  info("visiting BlockStatement")
  for node in node.statements:
    visitor.visitStatement(node)
  info("exiting BlockStatement")

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  info("visiting AssignmentStatement")

  visitor.visitExpression(node.left)
  visitor.visitExpecting(node.value, node.left.returnType)
  
  if node.left.returnType.kind.eq(typeStaticArray) and node.value.returnType.kind.eq(typeStaticArray):
    if node.left.returnType.length < node.value.returnType.length:
      newError(errSize, node.value.token, @{"@0": $node.value.returnType, "@1": $node.left.returnType})
    else:
      node.value.returnType.length = node.left.returnType.length
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

  elif node.symbolType.kind.eq(typeStaticArray) and node.symbolType.length == 0:
    newError(errEmptyStaticArray, node.name)

  else:
    visitor.newSymbol(node.name, node.symbolType)

  info("exiting DefaultStatement")

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
  warn("argument does not exist with key: ", key)
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

      visitor.visitExpression(expr)

      node.setType(getPtrType(expr.returnType))

    of skArr: 
      info("Semantic analysis of skArr special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      var expected = getUndefinedType()
      if visitor.expectedContextType.kind.eq typeArray:
        expected = getStaticArrayType(visitor.expectedContextType.arrBase, 0)
      else:
        warn("non-array context")

      visitor.visitExpecting(expr, expected)
      if not node.expect("0", typeStaticArray): break analysis

      if expr of TypeExpression:
        node.add("@", newBoolExpression(expr.token.newFrom(kind = tkTrue)))

      node.setType(getArrayType(expr.returnType.staticArrBase))

    of skLen:
      info("Semantic analysis of skLen special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      visitor.visitExpression(expr)
      if node.expect("0", typeArray, typeStaticArray): break analysis

      node.setType(getInt64Type())

    else:
      warn("Unhandled special expression: ", node.kind)

  info("exiting SpecialExpression")

method visitSpecialStatement*(visitor: SemanticAnalyzerVisitor, node: SpecialStatement): auto =
  info("visiting SpecialStatement")

  block analysis:
    case node.kind:
    of skPrint:
      info("Semantic analysis of skPrint special")
      node.checkUnexpected(expected = @["0", "term"])
      let expr = node.get("0")

      visitor.visitExpecting(expr, getArrayType(getCharType()))
      if not node.expect("0", getArrayType(getCharType())): break analysis

      if node.has("term"):
        visitor.visitExpecting(node.get("term"), getStaticArrayType(getCharType(), 0))
        if not node.expect("term", getStaticArrayType(getCharType(), 0)): break analysis

    of skFree:
      info("Semantic analysis of skPrint special")
      node.checkUnexpected(expected = @["0"])
      let expr = node.get("0")

      visitor.visitExpression(expr)
      if not node.expect("0", typeArray, typePtr): break analysis

    of skAssert:
      info("Semantic analysis of skPrint special")
      node.checkUnexpected(expected = @["0", "1"])
      let cond = node.get("0")
      visitor.visitExpecting(cond, getBoolType())

      if not node.expect("0", getBoolType()): break analysis

      if node.has("1"):
        visitor.visitExpecting(node.get("1"), getStaticArrayType(getCharType(), 0))
        if not node.expect("1", getStaticArrayType(getCharType(), 0)): break analysis

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
  else:
    warn("unhandled statement")