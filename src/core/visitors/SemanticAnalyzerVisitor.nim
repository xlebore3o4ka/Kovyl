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

  FormEntry = ref object
    form: FormStatement
    instances: Table[string, FuncStatement]
    scopeDepth: Natural

  SemanticAnalyzerVisitor* = ref object of Visitor
    stdLibPath*: string

    currentScope: Scope
    symbolScopeStack: Table[string, seq[Scope]]

    expectedContextType: Type
    loopLevel: Natural
    funcStack: seq[FuncStatement]
    moduleCache: Table[string, Type]

    formTable: Table[string, seq[FormEntry]]
    moduleFormTable: Table[string, Table[string, seq[FormEntry]]]

    nodeStack: seq[string]

proc log(self: SemanticAnalyzerVisitor, args: varargs[string, `$`]) =
  let prefix = $self.currentScope.depth
  let indent = prefix.alignLeft(4)
  
  let stackIndent = "| ".repeat(self.nodeStack.len - 1)
  let mainPart = indent & " " & $self.nodeStack[^1] & " " & stackIndent & args.join("")
  
  const LOG_WIDTH = 150
  
  if self.expectedContextType != getUndefinedType():
    let contextType = " [" & $self.expectedContextType & "]"
    let padding = max(1, LOG_WIDTH - mainPart.len - contextType.len)
    info(mainPart, " ".repeat(padding), contextType)
  else:
    info(mainPart)

proc setType(expr: Expression, returnType: Type, self: SemanticAnalyzerVisitor) {.inline.} =
  expr.returnType = returnType
  self.log("Return type is set as: ", $returnType)

proc trySetNumber*(node: BinaryExpression, visitor: SemanticAnalyzerVisitor): bool {.inline.} =
  if node.left.returnType.isNumber and node.right.returnType.eq node.left.returnType: 
    if node.token.kind in {tkPlus, tkMinus, tkStar, tkSlash, tkPercent}: 
      node.setType(node.left.returnType, visitor); return true
    elif node.token.kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}: 
      node.setType(getBoolType(), visitor); return true
  return false

proc trySetChar*(node: BinaryExpression, visitor: SemanticAnalyzerVisitor): bool {.inline.} =
  if node.left.returnType.eq(typeChar) and node.right.returnType.eq node.left.returnType: 
    if node.token.kind in {tkPlus, tkMinus, tkStar, tkSlash, tkPercent}: 
      node.setType(node.left.returnType, visitor); return true
    elif node.token.kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}: 
      node.setType(getBoolType(), visitor); return true
  return false

var logger = newFileLogger("KOVYLsemanticAnalyzer.log.kovyl", fmtStr = "KOVYL [SemanticAnalyzer] $levelname: ", mode = fmWrite)

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
  self.visitExpression(expr)
  self.expectedContextType = context

proc coerce(self: SemanticAnalyzerVisitor, left: Expression, right: Expression, expected: Type): bool =
  self.log("attempt to coerce types")
  self.visitExpecting(left, expected)
  var arrays = false

  if left.returnType.eq(getVecType(getCharType())) and left.returnType.kind != typeVar:
    self.visitExpecting(right, getArrayType(left.returnType.vecBase, 0))
    arrays = true
  else:
    self.visitExpecting(right, left.returnType)

  if left.returnType.neq(right.returnType):
    if right.returnType.eq(getVecType(getCharType())) and right.returnType.kind != typeVar:
      self.visitExpecting(left, getArrayType(right.returnType.vecBase, 0))
      arrays = true
    else:
      self.visitExpecting(left, right.returnType)

  return left.returnType.eq(right.returnType) or arrays

proc newSymbol(self: SemanticAnalyzerVisitor, name: Token, symbolType: Type, pub: bool) =
  self.currentScope.symbolTable[name.lexeme] = Symbol(token: name, symbolType: symbolType, pub: pub)
  self.symbolScopeStack.mgetOrPut(name.lexeme, @[]).add(self.currentScope)
  self.log((if pub: "Public s" else: "S") & "ymbol created: ", name.lexeme, " of type ", $symbolType, 
    " at the depth of the scope: ", $self.currentScope.depth)

proc pushScope(self: SemanticAnalyzerVisitor) =
  let depth = self.currentScope.depth
  self.currentScope = Scope(isGlobal: false, symbolTable: initTable[string, Symbol](), 
    parent: self.currentScope, depth: depth + 1)
  self.log("Scope pushed (", depth, " -> ", self.currentScope.depth, ")")

proc popScope(self: SemanticAnalyzerVisitor) =
  let scope = self.currentScope
  for name in scope.symbolTable.keys:
    discard self.symbolScopeStack[name].pop()
    self.log("Symbol removed: ", name)
  self.currentScope = scope.parent
  self.log("Scope popped (", scope.depth, " -> ", self.currentScope.depth, ")")

proc getSymbol(self: SemanticAnalyzerVisitor, name: string): Symbol =
  let scope = self.symbolScopeStack[name][^1]
  result = scope.symbolTable[name]
  self.log("got symbol: " & result.token.lexeme & " of type " & $result.symbolType, 
    " at the depth of the scope: ", $scope.depth, " (current: ", $self.currentScope.depth, ")")

proc symbolExists(self: SemanticAnalyzerVisitor, name: string): bool =
  let exists = name in self.symbolScopeStack and self.symbolScopeStack[name].len > 0
  self.log("checking symbol existence: " & name & " -> " & $exists)
  return exists

proc symbolExistsInCurrentScope(self: SemanticAnalyzerVisitor, name: string): bool =
  let exists = name in self.currentScope.symbolTable
  self.log("checking symbol existence in current scope: " & name & " -> " & $exists)
  return exists

proc overload(self: SemanticAnalyzerVisitor, name: string, overloadType: Type) =
  let scope = self.symbolScopeStack[name][^1]
  scope.symbolTable[name].symbolType.overloads[name & $overloadType] = overloadType
  self.log("Function ", name, " overloaded as ", overloadType)

# EXPRESSIONS

method visitNumberExpression*(visitor: SemanticAnalyzerVisitor, node: NumberExpression): auto =
  node.setType(inferNumberType(node, visitor.expectedContextType), visitor)

method visitNulExpression*(visitor: SemanticAnalyzerVisitor, node: NulExpression): auto =
  if visitor.expectedContextType.kind in {typePtr, typeVec}:
    node.setType(visitor.expectedContextType, visitor)
  else:
    warn("Nul in non-pointer context")

method visitBinaryExpression*(visitor: SemanticAnalyzerVisitor, node: BinaryExpression): auto =
  let left = node.left
  let right = node.right

  if not visitor.coerce(left, right, visitor.expectedContextType):
    warn("coercion failed")
    node.newBinaryTypeMismatchError()

  elif left.returnType.kind.eq(right.returnType.kind) and left.returnType.kind.eq(typeVar):
    node.setType(getBoolType(), visitor)
  elif node.trySetNumber(visitor):              discard
  elif node.trySetChar(visitor):                discard
  elif node.checkEqNeq(typeChar):        node.setType(getBoolType(), visitor)
  elif node.checkEqNeq(typeArray):       node.setType(getBoolType(), visitor)
  elif node.checkEqNeq(typeVec):         node.setType(getBoolType(), visitor)
  elif node.checkEqNeq(typePtr):         node.setType(getBoolType(), visitor)
  elif node.checkEqNeq(typeTuple):       node.setType(getBoolType(), visitor)
  elif node.checkAndOr():                node.setType(getBoolType(), visitor)
  elif node.checkEqNeqStrings():         node.setType(getBoolType(), visitor)
  else:                                  node.newBinaryTypeMismatchError()

method visitUnaryExpression*(visitor: SemanticAnalyzerVisitor, node: UnaryExpression): auto =
  visitor.visitExpression(node.value)

  if   node.checkPlusMinus(): node.setType(node.value.returnType, visitor)
  elif node.checkNot():       node.setType(getBoolType(), visitor)
  else:                       node.newUnaryTypeMismatchError()

method visitIdentifierExpression*(visitor: SemanticAnalyzerVisitor, node: IdentifierExpression): auto =
  var error = false

  if not visitor.symbolExists(node.token.lexeme):
    newError(errUndeclaredSymbol, node.token, @{"@0": node.token.lexeme})
    error = true

  if not error:
    node.setType(visitor.getSymbol(node.token.lexeme).symbolType, visitor)

method visitCastExpression*(visitor: SemanticAnalyzerVisitor, node: CastExpression): auto =
  let valueType = node.value.returnType
  let to = node.returnType

  let illegal = valueType.kind in {typePtr, typeVec, typeBool} or to.kind in {typePtr, typeVec, typeBool} 

  visitor.log("Type conversion attempt (", valueType, " -> ", to, ") -> ", not illegal)

  if illegal:
    newError(errCannotCast, node.token, @{"@0": $valueType, "@1": $to})
    node.returnType = getUndefinedType()

method visitDerefExpression*(visitor: SemanticAnalyzerVisitor, node: DerefExpression): auto =
  var error = false

  visitor.visitExpecting(node.value, getPtrType(visitor.expectedContextType))
  if node.value.returnType.kind.neq typePtr:
    newError(errTypeMismatch, node.token, @{"@0": $typePtr, "@1": $node.value.returnType})
    error = true

  if not error:
    node.setType(node.value.returnType.ptrBase, visitor)

method visitArrayExpression*(visitor: SemanticAnalyzerVisitor, node: ArrayExpression): auto =
  var expected = getUndefinedType()
  var derived = getUndefinedType()
  var error = false

  for expr in node.values:
    visitor.visitExpecting(expr, derived)
    if not expr.returnType.eq(getUndefinedType()) and not expr.returnType.eq(getNulType()):
      derived = expr.returnType
      break
  visitor.log("type was derived from the array elements as ", derived)

  if visitor.expectedContextType.eq typeArray:
    expected = visitor.expectedContextType.arrBase
  else:
    visitor.log("non-array context")

  if expected.eq(typeArray) and derived.eq(typeArray):
    if expected.length > derived.length:
      derived = expected
  if expected != derived:
    expected = derived

  visitor.log("visiting ArrayExpression values...")
  for expr in node.values:
    visitor.visitExpecting(expr, expected)

    if expr.returnType.eq(typeArray) and expected.eq(typeArray):
      if expr.returnType.length > expected.length:
        newError(errSize, expr.token, @{"@0": $expr.returnType, "@1": $expected})
        error = true
        break
      expr.returnType = getArrayType(expr.returnType.arrBase, expected.length)
      visitor.log("The size of the static array '" & expr.token.lexeme & "' has been determined to " & $expected.length)

    elif expected.neq(getUndefinedType()) and expr.returnType.neq expected:
      newError(errTypeMismatch, expr.token, @{"@0": $expected, "@1": $expr.returnType})
      error = true
      break

  if not error:
    node.setType(getArrayType(expected, node.values.len), visitor)

method visitIndexExpression*(visitor: SemanticAnalyzerVisitor, node: IndexExpression): auto =
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
      (if node.value.returnType.kind.eq typeVec:   node.value.returnType.vecBase
       else:                                       node.value.returnType.arrBase), visitor)

method visitTupleExpression*(visitor: SemanticAnalyzerVisitor, node: TupleExpression): auto =
  var error = false

  if visitor.expectedContextType.neq typeTuple:
    warn("non-tuple context")
    var elements = node.elements
    var elementTypes = initOrderedTable[string, Type]()

    for nameToken, expr in elements.pairs:
      visitor.visitExpecting(expr, getUndefinedType())
      elementTypes[nameToken.lexeme] = expr.returnType

    node.setType(getTupleType(elementTypes), visitor)

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
      node.setType(visitor.expectedContextType, visitor)

method visitFieldExpression*(visitor: SemanticAnalyzerVisitor, node: FieldExpression): auto =
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

    visitor.log("field ", node.token.lexeme, " is correct")
    node.setType(fields[node.token.lexeme], visitor)

proc checkOverloads(visitor: SemanticAnalyzerVisitor, node: CallExpression, varType: Type): (bool, Type) =
  visitor.log("node is not first defined function")
  visitor.log("Creating a function type from arguments and context...")

  var arguments: OrderedTable[string, Type]
  for i, expr in node.arguments:
    visitor.visitExpression(expr)
    arguments[$i] = expr.returnType

  let funcType = getFuncType(arguments, visitor.expectedContextType)
  visitor.log("created type: ", funcType)

  block checkOverloads:
    visitor.log("checking overloads...")
    if varType.overloads.len == 0:
      warn("no overload was found")
      return (false, funcType)

    for _, overload in varType.overloads.pairs:
      visitor.log("checking overload ", overload, " equals ", funcType, "...")
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
        visitor.log("overload found with compatible arguments")
        node.setType(overload.returnType, visitor)
        node.value.setType(overload, visitor)
        return (true, funcType)

    warn("No matching overloads found for function ", varType.funcName)
    return (false, funcType)

proc checkFirstDefinedFunction(visitor: SemanticAnalyzerVisitor, node: CallExpression, varType: Type): (bool, bool, Type) =
  block checkDefault:
    visitor.log("checking node == first defined function...")
    if node.arguments.len != varType.arguments.len:
      warn("node arguments len != first defined function arguments len")
      return (false, false, getUndefinedType())

    if visitor.expectedContextType.neq varType.returnType:
      warn("expected context type != first defined function return type")
      let (found, funcType) = checkOverloads(visitor, node, varType)
      if found:
        return (false, true, funcType)

    for i, expr in node.arguments:
      let index = $i
      var expected = node.value.returnType.arguments[index]

      visitor.visitExpecting(expr, expected)

      if expected.kind.eq(typeArray) and expr.returnType.kind.eq(typeArray):
        if expected.arrBase.neq expr.returnType.arrBase:
          return (false, false, getUndefinedType())
        if expected.length < expr.returnType.length:
          return (false, false, getUndefinedType())
        elif expected.length > expr.returnType.length:
          expr.returnType = expected
          visitor.log("Array size promoted from ", expr.returnType.length, " to ", expected.length)

      if expr.returnType != expected:
        warn("argument types != first defined function argument types")
        return (false, false, getUndefinedType())

    visitor.log("node is first defined function")
    node.setType(node.value.returnType.returnType, visitor)
    return (true, false, getUndefinedType())

proc reportNoOverloadFound(visitor: SemanticAnalyzerVisitor, node: CallExpression, varType: Type, funcType: Type) =
  let funcName = node.value.token
  var avaiableOverloadFormatted: string = "- " & funcName.lexeme & $varType

  if node.value of InstanceExpression:
    if funcName.lexeme in visitor.formTable:
      for entry in visitor.formTable[funcName.lexeme]:
        let form = entry.form
        avaiableOverloadFormatted &= "\n- " & formToString(form)
  else:
    for name, _ in varType.overloads.pairs:
      avaiableOverloadFormatted &= "\n- " & name & $funcType

  newError(errFuncResolution, funcName, @{"@0": funcName.lexeme, "@1": $funcType, "@2": avaiableOverloadFormatted})

method visitCallExpression*(visitor: SemanticAnalyzerVisitor, node: CallExpression): auto =
  visitor.log("visiting CallExpression")

  visitor.visitExpression(node.value)
  var error = false
  let varType = node.value.returnType

  block checkAll:
    if varType.neq typeFunc:
      newError(errTypeMismatch, node.token, @{"@0": $typeFunc, "@1": $node.value.returnType})
      error = true
      break checkAll

    let (isFirst, isOverload, overloadType) = checkFirstDefinedFunction(visitor, node, varType)
    if isFirst:
      break checkAll
    elif isOverload:
      node.setType(overloadType, visitor)

    let (found, funcType) = checkOverloads(visitor, node, varType)
    if found:
      break checkAll

    reportNoOverloadFound(visitor, node, varType, funcType)

  visitor.log("exiting CallExpression")

proc monomorphizeForm(self: SemanticAnalyzerVisitor, form: FormStatement, types: seq[Type]): FuncStatement =
  var typeMap: Table[string, Type]

  for i in 0..<types.len:
    typeMap[form.formParams[i].lexeme] = types[i]

  var newArguments = initOrderedTable[string, FuncArgument]()
  for key, arg in form.arguments:
    var newArg = FuncArgument(
        origin: arg.origin,
        expectedType: arg.expectedType
    )
    for varName, replacement in typeMap:
      newArg.expectedType = substituteTypeVar(newArg.expectedType, varName, replacement)
    newArguments[key] = newArg

  var newReturnType = form.returnType
  for varName, replacement in typeMap:
    newReturnType = substituteTypeVar(newReturnType, varName, replacement)

  for key, arg in newArguments.pairs:
    self.log("monomorphized arg ", key, ": ", arg.expectedType)
  self.log("monomorphized return: ", newReturnType)

  let clonedBody = BlockStatement(cloneAst(form.formBlock))
  recursiveMonomorphization(clonedBody, typeMap)

  var argumentTypes = initOrderedTable[string, Type]()
  var index = 0
  for _, arg in newArguments.pairs:
    argumentTypes[$index] = arg.expectedType
    index.inc

  result = newFuncStatement(
    returnType = newReturnType,
    name = form.name.newFrom(lexeme = form.name.lexeme),
    arguments = newArguments,
    funcBlock = clonedBody,
    pub = form.pub
  )
  result.funcType = getFuncType(argumentTypes, newReturnType, form.name.lexeme & $getFuncType(argumentTypes, newReturnType))

  var errorsCount = errors.errors.len

  self.pushScope()

  result.funcClosures = form.closures.keys.toSeq
  for name, typ in form.closures:
    self.newSymbol(form.name.newFrom(lexeme = name), typ, false)

  self.visitFuncStatement(result)

  self.popScope()

  if errorsCount != errors.errors.len:
    return nil

proc instantiationFingerprint(types: seq[Type]): string =
  types.mapIt(typeFingerprint(it)).join("|")

proc processFormResolution(
  visitor: SemanticAnalyzerVisitor,
  node: InstanceExpression,
  formEntries: seq[FormEntry]
): bool =
  let typeKey = instantiationFingerprint(node.types)
  var foundAtLeastOne = false

  for entry in formEntries:
    let form = entry.form
    visitor.log("- with ", formToString(form))
    
    if form.formParams.len != node.types.len:
      visitor.log("param length mismatch")
      continue
    
    foundAtLeastOne = true

    if typeKey in entry.instances:
      let cached = entry.instances[typeKey]
      if cached == nil:
        visitor.log("cached error for ", typeKey)
        continue
      visitor.log("cache hit for ", typeKey)
      node.overloads[cached.name.lexeme] = cached
      if node.returnType.neq(getUndefinedType()):
        node.returnType.overloads[cached.name.lexeme] = cached.funcType
        visitor.log("The function was overloaded (cached)")
      else:
        node.setType(cached.funcType, visitor)
      continue

    visitor.log("suitable form has been found. Monomorphization...")
    let funcStatement = visitor.monomorphizeForm(form, node.types)

    if funcStatement == nil:
      entry.instances[typeKey] = nil
      newError(errMonomorphizationError, node.name, @{"@0": node.name.lexeme, "@1": "(" & node.types.mapIt($it).join(", ") & ")"})
      continue

    if visitor.symbolExists(funcStatement.name.lexeme):
      visitor.currentScope.symbolTable.del(funcStatement.name.lexeme)
      if funcStatement.name.lexeme in visitor.symbolScopeStack and visitor.symbolScopeStack[funcStatement.name.lexeme].len > 0:
        discard visitor.symbolScopeStack[funcStatement.name.lexeme].pop()

    funcStatement.funcClosures = form.closures.keys.toSeq
    for name, typ in form.closures:
      visitor.newSymbol(form.name.newFrom(lexeme = name), typ, false)
    visitor.visitFuncStatement(funcStatement)

    visitor.currentScope.symbolTable.del(funcStatement.name.lexeme)
    if funcStatement.name.lexeme in visitor.symbolScopeStack and visitor.symbolScopeStack[funcStatement.name.lexeme].len > 0:
      discard visitor.symbolScopeStack[funcStatement.name.lexeme].pop()

    entry.instances[typeKey] = funcStatement

    visitor.log("the function was successfully generated and cached: ", funcStatement.name.lexeme, funcStatement.funcType)
    node.overloads[funcStatement.name.lexeme] = funcStatement
    if node.returnType.neq(getUndefinedType()):
      node.returnType.overloads[funcStatement.name.lexeme] = funcStatement.funcType
      visitor.log("The function was overloaded")
    else:
      node.setType(funcStatement.funcType, visitor)

  if not foundAtLeastOne:
    if formEntries.len == 0:
      newError(errMonomorphizationError, node.name, @{"@0": node.name.lexeme, "@1": "(" & node.types.mapIt($it).join(", ") & ")"})
    else:
      var avaiableOverloadFormatted: string
      for n, entry in formEntries:
        if n != 0: avaiableOverloadFormatted &= "\n"
        avaiableOverloadFormatted &= "- " & formToString(entry.form)
      newError(errFormResolution, node.name, @{"@0": node.name.lexeme, "@1": "$[" & node.types.mapIt($it).join(", ") & "]", "@2": avaiableOverloadFormatted})
  
  return foundAtLeastOne

method visitInstanceExpression*(visitor: SemanticAnalyzerVisitor, node: InstanceExpression): auto =
  visitor.log("visiting InstanceExpression")
  
  block analysis:
    if node.module != nil:
      visitor.visitExpression(node.module)
      let returnType = node.module.returnType

      if returnType.neq typeModule:
        warn("getting a field from a fieldless type")
        newError(errFieldless, node.token, @{"@0": $node.module.returnType})

      elif returnType.modulePath in visitor.moduleFormTable and node.name.lexeme in visitor.moduleFormTable[returnType.modulePath]:
        let formEntries = visitor.moduleFormTable[returnType.modulePath][node.name.lexeme]
        discard processFormResolution(visitor, node, formEntries)

      else:
        newError(errHasNoField, node.module.token, @{"@0": node.module.token.lexeme, "@1": node.name.lexeme})

    elif node.name.lexeme in visitor.formTable:
      visitor.log("forms of ", node.name.lexeme, " was found in the table. Comparison")
      discard processFormResolution(visitor, node, visitor.formTable[node.name.lexeme])

    else:
      newError(errUndeclaredSymbol, node.name, @{"@0": node.name.lexeme})

  visitor.log("exiting InstanceExpression")

# STATEMENTS

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  var error = false

  var expected = node.symbolType

  visitor.visitExpecting(node.value, expected)

  var valueType = node.value.returnType

  if expected.kind.eq(typeArray) and valueType.kind.eq typeArray:
    if expected.length == 0 and valueType.length != 0:
      expected = getArrayType(expected.arrBase, valueType.length)
      visitor.log("The size of the static array '" & node.name.lexeme & "' has been determined to " & $valueType.length)
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
      visitor.log("The size of the static array '" & node.value.token.lexeme & "' has been determined to " & $valueType.length)

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

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  visitor.log("do")

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

  visitor.log("end")

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  visitor.visitExpression(node.left)
  visitor.visitExpecting(node.value, node.left.returnType)
  
  if node.left.returnType.kind.eq(typeArray) and node.value.returnType.kind.eq(typeArray):
    if node.left.returnType.length < node.value.returnType.length:
      newError(errSize, node.value.token, @{"@0": $node.value.returnType, "@1": $node.left.returnType})
    else:
      node.value.returnType = getArrayType(node.value.returnType.arrBase, node.left.returnType.length)
      visitor.log("The size of the static array '" & node.left.token.lexeme & 
        "' has been determined to " & $node.value.returnType.length)

  elif node.left.returnType.neq node.value.returnType:
    newError(errTypeMismatch, node.left.token, @{"@0": $node.left.returnType, "@1": $node.value.returnType})

method visitBranchingStatement*(visitor: SemanticAnalyzerVisitor, node: BranchingStatement): auto =
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

method visitBreakStatement*(visitor: SemanticAnalyzerVisitor, node: BreakStatement): auto =
  visitor.log("Checking loop level -> ", visitor.loopLevel)
  if visitor.loopLevel == 0:
    newError(errForbiddenLocation, node.token)

method visitContinueStatement*(visitor: SemanticAnalyzerVisitor, node: ContinueStatement): auto =
  visitor.log("Checking loop level -> ", visitor.loopLevel)
  if visitor.loopLevel == 0:
    newError(errForbiddenLocation, node.token)

method visitWhileStatement*(visitor: SemanticAnalyzerVisitor, node: WhileStatement): auto =
  visitor.loopLevel.inc
  visitor.log("Incrementing loop level: ", visitor.loopLevel - 1, " -> ", visitor.loopLevel)
  visitor.pushScope()
  
  visitor.visitExpecting(node.condition, getBoolType())
  if node.condition.returnType.neq(getBoolType()):
    newError(errTypeMismatch, node.condition.token, @{"@0": $getBoolType(), "@1": $node.condition.returnType})
  
  visitor.visitStatement(node.whileBlock)
  
  visitor.popScope()
  visitor.loopLevel.dec
  visitor.log("Decrementing loop level: ", visitor.loopLevel + 1, " -> ", visitor.loopLevel)

method visitDefaultStatement*(visitor: SemanticAnalyzerVisitor, node: DefaultStatement): auto =
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

method visitFuncStatement*(visitor: SemanticAnalyzerVisitor, node: FuncStatement): auto =
  visitor.log("visiting FuncStatement")

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
    visitor.log("Checking that all paths in the function '", node.name.lexeme, "' block end with the return expression")
    if not blockEndsWithReturn(node.funcBlock):
      warn("...false")
      newError(errMissingReturn, node.name, @{"@0": node.name.lexeme})
      error = true
    else:
      visitor.log("...true")

  if not error:
    visitor.log("Checking function overloads...")
    if visitor.symbolExists(node.name.lexeme):
      var funcSymbol = visitor.getSymbol(node.name.lexeme)
      error = false

      if funcSymbol.symbolType.arguments == funcType.arguments:
        newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": funcSymbol.token.file,
            "@2": $funcSymbol.token.line, "@3": $funcSymbol.token.column})
        error = true

      for _, overType in funcSymbol.symbolType.overloads.pairs:
        if overType.arguments == funcType.arguments:
          newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": funcSymbol.token.file,
              "@2": $funcSymbol.token.line, "@3": $funcSymbol.token.column})
          error = true
          break

      if not error:
        visitor.overload(node.name.lexeme, 
          getFuncType(funcType.arguments, funcType.returnType, node.name.lexeme & $funcType))
        node.name.lexeme &= $funcType

    else:
      visitor.log("New function type is set as: ", funcType)
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

  visitor.log("exiting FuncStatement")

method visitReturnStatement*(visitor: SemanticAnalyzerVisitor, node: ReturnStatement): auto =
  visitor.log("Checking func level -> ", visitor.funcStack.len)
  if visitor.funcStack.len == 0:
    newError(errForbiddenLocation, node.token)

  if node.hasValue:
    visitor.visitExpecting(node.value, visitor.funcStack[^1].returnType)
    if node.value.returnType.neq visitor.funcStack[^1].returnType:
      newError(errTypeMismatch, node.value.token, @{"@0": $visitor.funcStack[^1].returnType, 
        "@1": $node.value.returnType})

  elif visitor.funcStack[^1].returnType.neq getUndefinedType():
      newError(errExpression, node.token, @{"@0": "return without expression"})

method visitForStatement*(visitor: SemanticAnalyzerVisitor, node: ForStatement): auto =
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

method visitCallStatement*(visitor: SemanticAnalyzerVisitor, node: CallStatement): auto =
  visitor.visitExpression(node.callExpression)
  let funcExpr = node.callExpression

  if funcExpr.returnType.neq getUndefinedType():
    newError(errUnusedReturn, funcExpr.value.token, @{"@0": funcExpr.value.token.lexeme})

method visitModuleStatement*(visitor: SemanticAnalyzerVisitor, node: ModuleStatement): auto =
  visitor.log("visiting ModuleStatement")

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
      else: joinPath(dir, modulePath) & ".kvl"
    )

    visitor.log("search for the ", node.path.lexeme, " module... (full path: ", fullPath, ")")

    if fullPath in visitor.moduleCache:
      visitor.log("found module ", node.path.lexeme, " in the module cache")
      node.moduleType = visitor.moduleCache[fullPath]
      node.fullPath = fullPath
      visitor.newSymbol(node.name, node.moduleType, false)
      break analysis

    if not fileExists(fullPath):
      warn("module ", node.path.lexeme, " does not exists")
      newError(errModuleNotFound, node.path, @{"@0": node.path.lexeme})
      break analysis

    visitor.log("found module ", node.path.lexeme, " on the path ", fullPath)

    let text = readFile(fullPath)

    var parser = newParser(text, fullPath)
    node.moduleBlock = parser.parse()

    if errors.errors.len != 0:
      newError(errCorruptedModule, node.name, @{"@0": node.path.lexeme})

    visitor.pushScope()
    visitor.log("semantic analysis of the ", node.path.lexeme, " module...")
    visitor.visitStatement(node.moduleBlock)

    visitor.log("creating a module type...")

    var symbols: OrderedTable[string, Type]
    for name, symbol in visitor.currentScope.symbolTable.pairs:
      if symbol.pub:
        visitor.log("Public symbol added to the module type: ", name)
        symbols[name] = symbol.symbolType
      else:
        visitor.log("Private symbol was skipped: ", name)

    for name, overloads in visitor.formTable.mpairs:
      var i = overloads.high
      while i >= 0:
        if overloads[i].scopeDepth == visitor.currentScope.depth:
          if overloads[i].form.pub:
            visitor.log("Public form added to the module type: ", name)
            visitor.moduleFormTable
              .mgetOrPut(fullPath, initTable[string, seq[FormEntry]]())
              .mgetOrPut(overloads[i].form.name.lexeme, newSeq[FormEntry]())
              .add(overloads[i])
          else:
            visitor.log("Private form was skipped: ", name)
          overloads.delete(i)
        dec(i)

    visitor.popScope()

    if errors.errors.len != 0:
      newError(errCorruptedModule, node.name, @{"@0": node.path.lexeme})
    else:
      node.moduleType = getModuleType(fullPath, symbols)
      visitor.newSymbol(node.name, node.moduleType, false)
      visitor.moduleCache[fullPath] = node.moduleType
      node.fullPath = fullPath

  visitor.log("exiting ModuleStatement")

method visitClosureStatement*(visitor: SemanticAnalyzerVisitor, node: ClosureStatement): auto =
  visitor.log("checking function level -> ", visitor.funcStack.len)
  if visitor.funcStack.len == 0:
    newError(errForbiddenLocation, node.token)

  else:
    var error = false

    for name in node.names:
      visitor.log("closing symbol ", name.lexeme, "...")
      if name.lexeme in visitor.funcStack[^1].funcClosures:
        visitor.log("already closed")
      elif not visitor.symbolExists(name.lexeme):
        newError(errUndeclaredSymbol, name, @{"@0": name.lexeme})
        error = true
      else:
        let symbol = visitor.getSymbol(name.lexeme)
        visitor.funcStack[^1].funcClosures.add(name.lexeme)
        visitor.log("symbol ", name.lexeme, " added to function ", visitor.funcStack[^1].name.lexeme, " closures")

        if symbol.symbolType.eq typeFunc:
          visitor.log("closing ", name.lexeme, " overloads...")
          for name, overload in symbol.symbolType.overloads.pairs:
            visitor.funcStack[^1].funcClosures.add(name)
            visitor.log("overload ", name, " added to function ", 
              visitor.funcStack[^1].name.lexeme, " closures")

method visitFormStatement*(visitor: SemanticAnalyzerVisitor, node: FormStatement): auto =
  visitor.log("visiting FormStatement")

  var error = false

  visitor.log("finding form overloads...")
  if node.name.lexeme in visitor.formTable:
    visitor.log("equality check self signature ", formToString(node))
    for entry in visitor.formTable[node.name.lexeme]:
      let form = entry.form
      visitor.log("- with ", formToString(form))
      if node.formParams.len != form.formParams.len: visitor.log("form params length mismatch"); continue
      if node.arguments.len  != form.arguments.len:  visitor.log("arguments length mismatch");   continue
      var cont = false
      for index in 0..<node.formParams.len:
        if node.formParams[index].lexeme != form.formParams[index].lexeme: 
          visitor.log("param mismatch")
          cont = true
          break
      if cont: continue
      if node.returnType.neq(form.returnType): continue
      for index in 0..<node.arguments.len:
        if node.arguments[$index].expectedType.neq form.arguments[$index].expectedType: 
          visitor.log("argument mismatch")
          cont = true
          break
      if cont: continue
      warn("a match was found")
      error = true
      newError(errRedeclaration, node.name, @{"@0": node.name.lexeme, "@1": form.name.file, "@2": $form.name.line, "@3": $form.name.column})
      break
    if not error:
      visitor.log("no matches found")
      
  else:
    visitor.log("not found")
  
  if not error:
    var funcArgs = initOrderedTable[string, FuncArgument]()
    for k, v in node.arguments:
      funcArgs[k] = FuncArgument(origin: v.origin, expectedType: v.expectedType)

    let funcNode = newFuncStatement(
      returnType = node.returnType,
      name = node.name,
      arguments = funcArgs,
      funcBlock = node.formBlock,
      pub = node.pub
    )

    let entry = FormEntry(form: node, instances: initTable[string, FuncStatement](), scopeDepth: visitor.currentScope.depth)
    visitor.formTable.mgetOrPut(node.name.lexeme, newSeq[FormEntry]()).add(entry)
    visitor.log(node.name.lexeme, " was added or overloaded to the form table")

    var errorsCount = errors.errors.len

    visitor.visitFuncStatement(funcNode)

    if errorsCount != errors.errors.len:
      discard visitor.formTable[node.name.lexeme].pop()
      visitor.log(node.name.lexeme, " was removed from the form table")
    else:
      if funcNode.name.lexeme in visitor.currentScope.symbolTable:
        visitor.currentScope.symbolTable.del(funcNode.name.lexeme)
        discard visitor.symbolScopeStack[funcNode.name.lexeme].pop()
      elif node.name.lexeme in visitor.currentScope.symbolTable:
        visitor.currentScope.symbolTable.del(node.name.lexeme)
        discard visitor.symbolScopeStack[node.name.lexeme].pop()

      for name in funcNode.funcClosures:
        node.closures[name] = visitor.getSymbol(name).symbolType

# SPECIALS

proc checkUnexpected(self: SpecialExpression | SpecialStatement, expected: seq[string], visitor: SemanticAnalyzerVisitor) =
  visitor.log("checking for unexpected arguments in special")

  for token, _ in self.namedArgs.pairs:
    let key = token.lexeme
    if key notin expected:
      if token.kind == tkIdentifier:
        warn("unexpected named argument found: ", key)
        newError(errUnexpectedNamedArgument, token, @{"@0": key})
      else:
        warn("unexpected argument found: ", key)
        newError(errUnexpectedArgument, token, @{"@0": key})

proc get(self: SpecialExpression | SpecialStatement, key: string, visitor: SemanticAnalyzerVisitor): Expression =
  visitor.log("getting argument with key: ", key, "...")
  for token, expr in self.namedArgs.pairs:
    let k = if token.kind == tkNumber: token.lexeme else: token.lexeme
    if k == key:
      visitor.log("argument found for key: ", key)
      return expr
  warn("argument not found for key: ", key)
  newError(errMissingArgument, self.token, @{"@0": key})
  return newErrorExpression(self.token)

proc add(self: SpecialExpression | SpecialStatement, key: string, expr: Expression, visitor: SemanticAnalyzerVisitor) =
  visitor.log("adding argument with key: ", key, " and expression type: ", $expr.returnType)
  let token = tkIdentifier.newToken(key, self.token.file, self.token.line, self.token.column, self.token.offset)
  self.namedArgs[token] = expr

proc has(self: SpecialExpression | SpecialStatement, key: string, visitor: SemanticAnalyzerVisitor): bool =
  visitor.log("checking if argument exists with key: ", key, "...")
  for token, _ in self.namedArgs.pairs:
    let k = token.lexeme
    if k == key:
      visitor.log("argument exists with key: ", key)
      return true
  visitor.log("argument does not exist with key: ", key)
  return false

proc expect(self: SpecialExpression | SpecialStatement, key: string, visitor: SemanticAnalyzerVisitor, types: varargs[Type]): bool =
  visitor.log("expecting argument with key: ", key, " and types: ", types.mapIt($it).join(" | "), "...")
  let expr = self.get(key, visitor)
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
  
  visitor.log("argument '", key, "' has correct type: ", $expr.returnType)
  return true

proc expect(self: SpecialExpression | SpecialStatement, key: string, visitor: SemanticAnalyzerVisitor, types: varargs[TypeKind]): bool =
  visitor.log("expecting argument with key: ", key, " and types: ", types.mapIt($it).join(" | "), "...")
  let expr = self.get(key, visitor)
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
  
  visitor.log("argument '", key, "' has correct type: ", $expr.returnType)
  return true

method visitSpecialExpression*(visitor: SemanticAnalyzerVisitor, node: SpecialExpression): auto =
  visitor.log("visiting SpecialExpression")

  block analysis:
    case node.kind:
    of skNew: 
      visitor.log("Semantic analysis of skNew special")
      node.checkUnexpected(expected = @["0"], visitor)
      let expr = node.get("0", visitor)

      if visitor.expectedContextType.eq typePtr:
        visitor.visitExpecting(expr, visitor.expectedContextType.ptrBase)
      else:
        warn("non-ptr context")
        visitor.visitExpression(expr)

      node.setType(getPtrType(expr.returnType), visitor)

    of skVec: 
      visitor.log("Semantic analysis of skVec special")
      node.checkUnexpected(expected = @["0", "@"], visitor)
      let expr = node.get("0", visitor)

      var expected = getUndefinedType()
      if visitor.expectedContextType.kind.eq typeVec:
        expected = getArrayType(visitor.expectedContextType.vecBase, 0)
      else:
        warn("non-array context")

      visitor.visitExpecting(expr, expected)
      if not node.expect("0", visitor, typeArray): break analysis

      if expr of TypeExpression:
        node.add("@", newBoolExpression(expr.token.newFrom(kind = tkTrue)), visitor)

      node.setType(getVecType(expr.returnType.arrBase), visitor)

    of skLen:
      visitor.log("Semantic analysis of skLen special")
      node.checkUnexpected(expected = @["0"], visitor)
      let expr = node.get("0", visitor)

      visitor.visitExpression(expr)
      if not node.expect("0", visitor, typeVec, typeArray): break analysis

      node.setType(getInt64Type(), visitor)

    of skFmt:
      visitor.log("Semantic analysis of skFmt special")
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

      if node.has("sep", visitor):
        visitor.visitExpecting(node.get("sep", visitor), getArrayType(getCharType(), 0))
        if not node.expect("sep", visitor, getArrayType(getCharType(), 0)): break analysis

      if node.has("repr", visitor):
        visitor.visitExpecting(node.get("repr", visitor), getBoolType())
        if not node.expect("repr", visitor, getBoolType()): break analysis

      node.setType(getVecType(getCharType()), visitor)

    of skTake:
      visitor.log("Semantic analysis of skTake special")
      if visitor.expectedContextType.kind.neq(typeArray):
        newError(errUnknownSize, node.token)
        break analysis
      elif visitor.expectedContextType.length == 0:
        newError(errEmptyStaticArray, node.token)
        break analysis

      node.checkUnexpected(expected = @["0"], visitor)
      let expr = node.get("0", visitor)

      visitor.visitExpecting(expr, getVecType(visitor.expectedContextType.arrBase))
      if not node.expect("0", visitor, typeVec): break analysis

      node.add("length", newNumberExpression(node.token.newFrom(kind = tkNumber,
        lexeme = $visitor.expectedContextType.length)), visitor)

      node.setType(getArrayType(expr.returnType.vecBase, visitor.expectedContextType.length), visitor)

    of skTakeof:
      visitor.log("Semantic analysis of skTakeof special")
      node.checkUnexpected(expected = @["0", "1"], visitor)
      let typ = node.get("0", visitor)

      visitor.visitExpression(typ)
      if not node.expect("0", visitor, typeArray): break analysis

      elif not (typ of TypeExpression):
        newError(errTypeMismatch, typ.token, @{"@0": "type annotation", "@1": "Expression"})
        break analysis

      elif typ.returnType.length == 0:
        newError(errEmptyStaticArray, node.token)
        break analysis

      let expr = node.get("1", visitor)

      visitor.visitExpecting(expr, getVecType(typ.returnType.arrBase))
      if not node.expect("1", visitor, typeVec): break analysis

      node.add("length", newNumberExpression(node.token.newFrom(kind = tkNumber,
        lexeme = $typ.returnType.length)), visitor)

      node.setType(getArrayType(typ.returnType.arrBase, typ.returnType.length), visitor)

    of skRead:
      visitor.log("Semantic analysis of skRead special")
      node.setType(getVecType(getCharType()), visitor)

    of skDefault:
      visitor.log("Semantic analysis of skDefault special")
      node.checkUnexpected(expected = @["0"], visitor)
      let typ = node.get("0", visitor)

      visitor.visitExpression(typ)

      if not (typ of TypeExpression):
        newError(errTypeMismatch, typ.token, @{"@0": "type annotation", "@1": "Expression"})
        break analysis

      node.setType(typ.returnType, visitor)

    else:
      warn("Unhandled special expression: ", node.kind)

  visitor.log("exiting SpecialExpression")

method visitSpecialStatement*(visitor: SemanticAnalyzerVisitor, node: SpecialStatement): auto =
  visitor.log("visiting SpecialStatement")

  block analysis:
    case node.kind:
    of skPrint:
      visitor.log("Semantic analysis of skPrint special")
      node.checkUnexpected(expected = @["0", "term", "free"], visitor)
      let expr = node.get("0", visitor)

      visitor.visitExpecting(expr, getVecType(getCharType()))
      if not node.expect("0", visitor, getVecType(getCharType())): break analysis

      if node.has("term", visitor):
        visitor.visitExpecting(node.get("term", visitor), getArrayType(getCharType(), 0))
        if not node.expect("term", visitor, getArrayType(getCharType(), 0)): break analysis

      if node.has("free", visitor):
        visitor.visitExpecting(node.get("free", visitor), getBoolType())
        if not node.expect("free", visitor, getBoolType()): break analysis

    of skFree:
      visitor.log("Semantic analysis of skFree special")
      node.checkUnexpected(expected = @["0"], visitor)
      let expr = node.get("0", visitor)

      visitor.visitExpression(expr)
      if not node.expect("0", visitor, typeVec, typePtr): break analysis

    of skAssert:
      visitor.log("Semantic analysis of skAssert special")
      node.checkUnexpected(expected = @["0", "1"], visitor)
      let cond = node.get("0", visitor)

      visitor.visitExpecting(cond, getBoolType())
      if not node.expect("0", visitor, getBoolType()): break analysis

      if node.has("1", visitor):
        visitor.visitExpecting(node.get("1", visitor), getArrayType(getCharType(), 0))
        if not node.expect("1", visitor, getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

    of skResize:
      visitor.log("Semantic analysis of skResize special")
      node.checkUnexpected(expected = @["0", "1"], visitor)
      let value = node.get("0", visitor)
      let size = node.get("1", visitor)
      
      visitor.visitExpression(value)
      if value.returnType.neq typeVec: 
        newError(errTypeMismatch, value.token, @{"@0": $typeVec, "@1": $value.returnType})
        break analysis

      visitor.visitExpression(size)
      if not node.expect("1", visitor, getInt64Type()): break analysis

    of skPanic:
      visitor.log("Semantic analysis of skPanic special")
      node.checkUnexpected(expected = @["0", "1"], visitor)
      let panicCode = node.get("0", visitor)
      let msg = node.get("1", visitor)

      visitor.visitExpecting(panicCode, getArrayType(getCharType(), 0))
      if not node.expect("0", visitor, getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

      visitor.visitExpecting(msg, getArrayType(getCharType(), 0))
      if not node.expect("1", visitor, getArrayType(getCharType(), 0), getVecType(getCharType())): break analysis

    else:
      warn("Unhandled special statement: ", node.kind)

  visitor.log("exiting SpecialStatement")

# GENERAL

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) =
  visitor.nodeStack.add(node.kind)

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
  elif node of InstanceExpression:
    visitor.visitInstanceExpression(InstanceExpression(node))
  else:
    warn("unhandled expression")

  discard visitor.nodeStack.pop()

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) =
  visitor.nodeStack.add(node.kind)

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
  elif node of FormStatement:
    visitor.visitFormStatement(FormStatement(node))
  else:
    warn("unhandled statement")

  discard visitor.nodeStack.pop()