import ../[astnodes, tokens, types, errors]
import visitor
import std/[tables, strutils]

type
  Symbol = object
    token: Token
    symbolType: Type
    id: Natural
    scopeLevel: Natural

  Scope = object
    startIndex: Natural
    addedNames: seq[string]

  SemanticAnalyzerVisitor* = ref object of Visitor
    symbolTable: Table[string, Natural]
    scopedSymbolPool: seq[Symbol]
    idToIndex: Table[Natural, Natural]
    scopeStack: seq[Scope]
    nextId: Natural = 0
    history: seq[tuple[name: string, oldId: Natural]]

    loopIndex: Natural = 0
    expectedContextType: Type

proc newSymbol(visitor: SemanticAnalyzerVisitor, token: Token, symbolType: Type) =
  let id = visitor.nextId
  visitor.nextId.inc
  
  let symbol = Symbol(
    token: token,
    symbolType: symbolType,
    id: id,
    scopeLevel: visitor.scopeStack.len
  )
  
  let poolIndex = visitor.scopedSymbolPool.len
  visitor.scopedSymbolPool.add(symbol)
  visitor.idToIndex[id] = poolIndex
  
  let oldId = visitor.symbolTable.getOrDefault(token.lexeme, high(Natural))
  visitor.history.add((token.lexeme, oldId))
  visitor.symbolTable[token.lexeme] = id
  
  visitor.scopeStack[^1].addedNames.add(token.lexeme)

proc getSymbol(visitor: SemanticAnalyzerVisitor, name: string): Symbol =
  let id = visitor.symbolTable[name]
  let poolIndex = visitor.idToIndex[id]
  return visitor.scopedSymbolPool[poolIndex]

proc pushScope(visitor: SemanticAnalyzerVisitor) =
  let scope = Scope(startIndex: visitor.scopedSymbolPool.len, addedNames: @[])
  visitor.scopeStack.add(scope)

proc popScope(visitor: SemanticAnalyzerVisitor) =
  let scope = visitor.scopeStack.pop()
  
  for name in scope.addedNames:
    let oldId = visitor.history.pop().oldId
    if oldId == high(Natural):
      visitor.symbolTable.del(name)
    else:
      visitor.symbolTable[name] = oldId
  
  visitor.scopedSymbolPool.setLen(scope.startIndex)

proc symbolExistsInCurrentScope(visitor: SemanticAnalyzerVisitor, name: string): bool =
  let scope = visitor.scopeStack[^1]
  for i in scope.startIndex ..< visitor.scopedSymbolPool.len:
    if visitor.scopedSymbolPool[i].token.lexeme == name:
      return true
  return false

proc newSemanticAnalyzerVisitor*(): SemanticAnalyzerVisitor =
  result = SemanticAnalyzerVisitor(expectedContextType: getUndefinedType())
  result.pushScope()

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) {.base.}

method visitNumberExpression*(visitor: SemanticAnalyzerVisitor, node: NumberExpression): auto =
  if (visitor.expectedContextType.kind == typeInt64 and isValidInt64(node.token.lexeme) or
      visitor.expectedContextType.kind == typeInt32 and isValidInt32(node.token.lexeme) or
      visitor.expectedContextType.kind == typeInt16 and isValidInt16(node.token.lexeme) or
      visitor.expectedContextType.kind == typeInt8 and isValidInt8(node.token.lexeme) or
      visitor.expectedContextType.kind == typeUInt64 and isValidUint64(node.token.lexeme) or
      visitor.expectedContextType.kind == typeUInt32 and isValidUint32(node.token.lexeme) or
      visitor.expectedContextType.kind == typeUInt16 and isValidUint16(node.token.lexeme) or
      visitor.expectedContextType.kind == typeUInt8 and isValidUint8(node.token.lexeme)):
    node.returnType = visitor.expectedContextType
  elif isNumber(visitor.expectedContextType):
    newError(errNumberLiteral, node.token, @{"@0": node.token.lexeme, "@1": $visitor.expectedContextType.kind})

method visitBinaryExpression*(visitor: SemanticAnalyzerVisitor, node: BinaryExpression): auto =
  visitor.visitExpression(node.left)

  let temp = visitor.expectedContextType
  visitor.expectedContextType = node.left.returnType

  visitor.visitExpression(node.right)

  visitor.expectedContextType = temp

  if isNumber(node.left.returnType) and isNumber(node.right.returnType) and 
        node.left.returnType != node.right.returnType:
    let temp = visitor.expectedContextType
    visitor.expectedContextType = node.right.returnType

    visitor.visitExpression(node.left)

    visitor.expectedContextType = temp

  case node.token.kind:
  of tkPlus, tkMinus, tkStar, tkSlash, tkPercent:
    if isNumber(node.left.returnType) and isNumber(node.right.returnType) and 
        node.left.returnType == node.right.returnType:
      node.returnType = node.left.returnType

  of tkGT, tkLT, tkGTE, tkLTE:
    if isNumber(node.left.returnType) and isNumber(node.right.returnType) and 
        node.left.returnType == node.right.returnType:
      node.returnType = getBoolType()

  of tkEQ, tkNEQ:
    if isNumber(node.left.returnType) and isNumber(node.right.returnType) and 
        node.left.returnType == node.right.returnType:
      node.returnType = getBoolType()
      
    if node.left.returnType == getCharType() and node.right.returnType == getCharType():
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typePtr and node.right.returnType.kind == typePtr:
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typeArray and node.right.returnType.kind == typeArray:
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typePtr and node.right.returnType.kind == typeNul:
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typeArray and node.right.returnType.kind == typeNul:
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typeNul and node.right.returnType.kind == typePtr:
      node.returnType = getBoolType()

    elif node.left.returnType.kind == typeNul and node.right.returnType.kind == typeArray:
      node.returnType = getBoolType()

  of tkAnd, tkOr:
    if node.left.returnType == getBoolType() and node.right.returnType == getBoolType():
      node.returnType = getBoolType()

  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled binary operator " & node.token.mean()

  if node.returnType == getUndefinedType():
    newError(errBinaryTypeMismatch, node.token, @{
        "@0": $node.token.lexeme, "@1": $node.left.returnType, "@2": $node.right.returnType})

method visitUnaryExpression*(visitor: SemanticAnalyzerVisitor, node: UnaryExpression): auto =
  visitor.visitExpression(node.operand)

  case node.token.kind:
  of tkMinus:
    if node.operand.returnType == getInt64Type():
      node.returnType = getInt64Type()
  of tkPlus:
    if node.operand.returnType == getUint64Type():
      node.returnType = getUint64Type()
    elif node.operand.returnType == getInt64Type():
      node.returnType = getInt64Type()
  of tkNot:
    if node.operand.returnType == getBoolType():
      node.returnType = getBoolType()
  else: 
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled unary operator " & node.token.mean()

  if node.returnType == getUndefinedType():
    newError(errUnaryTypeMismatch, node.token, @{
      "@0": $node.token.lexeme, "@1": $node.operand.returnType})

method visitIdentifierExpression*(visitor: SemanticAnalyzerVisitor, node: IdentifierExpression): auto =
  if node.token.lexeme notin visitor.symbolTable:
    newError(errUndeclaredSymbol, node.token, @{"@0": node.token.lexeme})
    return

  node.returnType = visitor.getSymbol(node.token.lexeme).symbolType

method visitCastExpression*(visitor: SemanticAnalyzerVisitor, node: CastExpression): auto =
  visitor.visitExpression(node.value)

  if node.returnType.kind in {typePtr, typeArray, typeNul}:
    newError(errProhibitedType, node.token, @{"@0": $node.returnType})
    node.returnType = getUndefinedType()

method visitDerefExpression*(visitor: SemanticAnalyzerVisitor, node: DerefExpression): auto =
  visitor.visitExpression(node.operand)
  
  if node.operand.returnType.kind != typePtr:
    newError(errTypeMismatch, node.token, @{"@0": "ptr", "@1": $node.operand.returnType})
    return
  
  if node.returnType == getUndefinedType():
    node.returnType = node.operand.returnType.ptrBaseType

method visitArrayExpression*(visitor: SemanticAnalyzerVisitor, node: ArrayExpression): auto =
  if node.values.len == 0:
    node.returnType = getStaticArrayType(visitor.expectedContextType, 0)
    return

  for val in node.values:
    visitor.visitExpression(val)
  
  var i = 0
  var firstType: Type
  while (firstType = node.values[i].returnType; firstType == getNulType()):
    i.inc

  for val in node.values:
    if firstType.kind == typePtr and val.returnType == getNulType():
      continue

    if val.returnType != firstType:
      newError(errTypeMismatch, val.token, @{"@0": $firstType, "@1": $val.returnType})
  
  node.returnType = getStaticArrayType(firstType, node.values.len)

method visitIndexExpression*(visitor: SemanticAnalyzerVisitor, node: IndexExpression): auto =
  visitor.visitExpression(node.operand)
  visitor.visitExpression(node.index)
  
  if not isNumber(node.index.returnType):
    newError(errTypeMismatch, node.index.token, @{"@0": "number", "@1": $node.index.returnType})
    return
  
  if node.operand.returnType.kind == typeArray:
    node.returnType = node.operand.returnType.arrayBaseType
  elif node.operand.returnType.kind == typeStaticArray:
    node.returnType = node.operand.returnType.staticArrayBaseType
  else:
    newError(errTypeMismatch, node.operand.token, @{"@0": $typeArray & " or " & $typeStaticArray, 
      "@1": $node.operand.returnType})
    return

# STATEMENTS

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) {.base.}

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  var varType = node.varType

  visitor.expectedContextType = getPrimitiveType(varType)

  visitor.visitExpression(node.value)

  visitor.expectedContextType = getUndefinedType()

  if visitor.symbolExistsInCurrentScope(node.name.lexeme):
    let name = visitor.getSymbol(node.name.lexeme).token
    newError(errRedeclaration, node.name, 
      @{"@0": node.name.lexeme, "@1": name.file, "@2": $name.line, "@3": $name.column}
    )
    varType = getUndefinedType()

  if node.varType.kind == typeStaticArray and node.varType.staticArrayLength == 0:
    if node.value.returnType.kind == typeStaticArray:
      node.varType = node.value.returnType

  if node.varType.kind == typeStaticArray and node.value.returnType.kind == typeStaticArray:
    if node.varType.staticArrayBaseType != node.value.returnType.staticArrayBaseType:
      newError(errTypeMismatch, node.name, @{"@0": $node.varType, "@1": $node.value.returnType})
      varType = getUndefinedType()
    elif node.value.returnType.staticArrayLength > node.varType.staticArrayLength:
      newError(errTypeMismatch, node.name, @{"@0": $node.varType, "@1": $node.value.returnType})
      varType = getUndefinedType()

  elif node.varType != node.value.returnType and not (
      node.varType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
    ):
    newError(errTypeMismatch, node.name, @{"@0": $node.varType, "@1": $node.value.returnType})
    varType = getUndefinedType()

  visitor.newSymbol(node.name, varType)

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  for stmt in node.statements:
    visitor.visitStatement(stmt)

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  if node.left of IdentifierExpression:
    let name = IdentifierExpression(node.left).token
    if name.lexeme notin visitor.symbolTable:
      newError(errUndeclaredSymbol, name, @{"@0": name.lexeme})
      return
    
    let varType = visitor.getSymbol(name.lexeme).symbolType

    visitor.expectedContextType = getPrimitiveType(varType)
    visitor.visitExpression(node.value)
    visitor.expectedContextType = getUndefinedType()

    if varType != node.value.returnType and not (
        varType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
      ):
      newError(errTypeMismatch, name, @{"@0": $varType, "@1": $node.value.returnType})
      return
  
  elif node.left of DerefExpression:
    visitor.visitExpression(DerefExpression(node.left).operand)

    let ptrType = DerefExpression(node.left).operand.returnType
    if ptrType.kind != typePtr:
      newError(errTypeMismatch, node.left.token, @{"@0": "ptr", "@1": $ptrType})
      return

    visitor.expectedContextType = getPrimitiveType(ptrType.ptrBaseType)
    visitor.visitExpression(node.value)
    visitor.expectedContextType = getUndefinedType()
      
    if ptrType.ptrBaseType != node.value.returnType and not (
        ptrType.ptrBaseType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
      ):
      newError(errTypeMismatch, node.left.token, @{"@0": $ptrType.ptrBaseType, "@1": $node.value.returnType})
      return

  elif node.left of IndexExpression:
    visitor.visitExpression(IndexExpression(node.left))
    
    let elemType = node.left.returnType

    visitor.expectedContextType = getPrimitiveType(elemType)
    visitor.visitExpression(node.value)
    visitor.expectedContextType = getUndefinedType()

    if elemType != node.value.returnType and not (
        elemType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
      ):
      newError(errTypeMismatch, node.left.token, @{"@0": $elemType, "@1": $node.value.returnType})
      return

method visitBranchingStatement*(visitor: SemanticAnalyzerVisitor, node: BranchingStatement): auto =
  visitor.pushScope()
  visitor.visitExpression(node.condition)
  
  if node.condition.returnType != getBoolType():
    newError(errTypeMismatch, node.condition.token, @{"@0": $typeBool, "@1": $node.condition.returnType})
  
  visitor.visitStatement(node.ifBlock)
  visitor.popScope()
  
  for el in node.elifBlocks:
    visitor.pushScope()
    visitor.visitExpression(el.cond)
    
    if el.cond.returnType != getBoolType():
      newError(errTypeMismatch, el.cond.token, @{"@0": $typeBool, "@1": $el.cond.returnType})
    
    visitor.visitStatement(el.elifBlock)
    visitor.popScope()
  
  if node.elseBlock != nil:
    visitor.pushScope()
    visitor.visitStatement(node.elseBlock)
    visitor.popScope()

method visitBreakStatement*(visitor: SemanticAnalyzerVisitor, node: BreakStatement): auto =
  if visitor.loopIndex == 0:
    newError(errForbiddenLocation, node.token)

method visitContinueStatement*(visitor: SemanticAnalyzerVisitor, node: ContinueStatement): auto =
  if visitor.loopIndex == 0:
    newError(errForbiddenLocation, node.token)

method visitWhileStatement*(visitor: SemanticAnalyzerVisitor, node: WhileStatement): auto =
  visitor.pushScope()
  visitor.loopIndex.inc
  defer:
    visitor.loopIndex.dec
    visitor.popScope()

  visitor.visitExpression(node.condition)
  
  if node.condition.returnType != getBoolType():
    newError(errTypeMismatch, node.condition.token, @{"@0": $typeBool, "@1": $node.condition.returnType})

  visitor.visitStatement(node.whileBlock)

# SPECIALS

proc checkUnexpected(self: SpecialExpression | SpecialStatement, expected: seq[string]) =
  for token, _ in self.namedArgs.pairs:
    let key = token.lexeme
    if key notin expected:
      if token.kind == tkIdentifier:
        newError(errUnexpectedNamedArgument, token, @{"@0": key})
      else:
        newError(errUnexpectedArgument, token, @{"@0": key})

proc get(self: SpecialExpression | SpecialStatement, key: string): Expression =
  for token, expr in self.namedArgs.pairs:
    let k = if token.kind == tkNumber: token.lexeme else: token.lexeme
    if k == key:
      return expr
  newError(errMissingArgument, self.token, @{"@0": key})
  return newErrorExpression(self.token)

proc expectNumber(self: SpecialExpression | SpecialStatement, key: string): bool =
  let expr = self.get(key)
  if expr of ErrorExpression: return false
  if not isNumber(expr.returnType):
    newError(errTypeMismatch, expr.token, @{"@0": "number", "@1": $expr.returnType})
    return false
  return true

proc add(self: SpecialExpression | SpecialStatement, key: string, expr: Expression) =
  let token = tkIdentifier.newToken(key, self.token.file, self.token.line, self.token.column, self.token.offset)
  self.namedArgs[token] = expr

proc has(self: SpecialExpression | SpecialStatement, key: string): bool =
  for token, _ in self.namedArgs.pairs:
    let k = token.lexeme
    if k == key:
      return true
  return false

proc allow(self: SpecialExpression | SpecialStatement, key: string, typ: Type) =
  if self.has(key):
    let expr = self.get(key)
    if expr.returnType != typ:
      newError(errTypeMismatch, expr.token, @{"@0": $typ, "@1": $expr.returnType})

proc expect(self: SpecialExpression | SpecialStatement, key: string, typ: Type): bool =
  let expr = self.get(key)
  if expr of ErrorExpression: return false
  if expr.returnType != typ:
    newError(errTypeMismatch, expr.token, @{"@0": $typ, "@1": $expr.returnType})
    return false
  return true

method visitSpecialExpression*(visitor: SemanticAnalyzerVisitor, node: SpecialExpression): auto =
  for _, expr in node.namedArgs.pairs:
    visitor.visitExpression(expr)

  case node.kind:
  of skNew: 
    node.checkUnexpected(expected = @["0"])
    node.returnType = getPtrType(node.get("0").returnType)

  of skArr: 
    node.checkUnexpected(expected = @["0", "1"])
    
    if node.has("1"):
      if not node.expectNumber("1"): return
      let expr = node.get("0")
      if expr of TypeExpression:
        node.add("@", newBoolExpression(expr.token.newFrom(tkTrue)))
      node.returnType = getArrayType(expr.returnType)
    else:
      let expr = node.get("0")
      if expr of ArrayExpression:
        node.returnType = getArrayType(expr.returnType.staticArrayBaseType)
      else:
        newError(errTypeMismatch, expr.token, @{"@0": $typeArray, "@1": $expr.returnType})

  of skLen:
    node.checkUnexpected(expected = @["0"])
    if (let returnType = node.get("0").returnType; returnType).kind notin {typeArray, typeStaticArray}:
      newError(errTypeMismatch, node.token, @{"@0": $typeArray & "or" & $typeStaticArray, "@1": $returnType})
    node.returnType = getInt64Type()

  of skFmt:
    node.allow("sep", getArrayType(getCharType()))
    node.allow("repr", getBoolType())
    node.allow("escape", getBoolType())

    for token, expr in node.namedArgs.pairs:
      if token.kind != tkNumber and token.lexeme != "sep" and token.lexeme != "repr" and token.lexeme != "strip":
        let err = if token.kind == tkIdentifier: errUnexpectedNamedArgument
          else: errUnexpectedArgument
        newError(err, token, @{"@0": token.lexeme})
      if expr.returnType.kind in {typePtr, typeArray} and expr.returnType != getArrayType(getCharType()):
        newError(errProhibitedType, expr.token, @{"@0": $expr.returnType})

    node.returnType = getArrayType(getCharType())

  else: 
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled special expression"

method visitSpecialStatement*(visitor: SemanticAnalyzerVisitor, node: SpecialStatement): auto =
  for _, expr in node.namedArgs.pairs:
    visitor.visitExpression(expr)
  
  case node.kind:
  of skPrint:
    node.allow("term", getArrayType(getCharType()))
    for token, expr in node.namedArgs.pairs:
      if token.kind != tkNumber and token.lexeme != "term":
        let err = if token.kind == tkIdentifier: errUnexpectedNamedArgument
          else: errUnexpectedArgument
        newError(err, token, @{"@0": token.lexeme})
      if expr.returnType.kind in {typePtr, typeArray} and expr.returnType != getArrayType(getCharType()):
        newError(errProhibitedType, expr.token, @{"@0": $expr.returnType})

  of skFree:
    if (let returnType = node.get("0").returnType; returnType).kind notin {typeArray, typePtr}:
      newError(errProhibitedType, node.token, @{"@0": $returnType})

  of skAssert:
    node.checkUnexpected(expected = @["0", "1"])
    discard node.expect("0", getBoolType())
    node.allow("1", getArrayType(getCharType()))
      
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled special statement"

# GENERAL

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) =
  if node of ErrorExpression: discard
  elif node of NumberExpression: 
    visitor.visitNumberExpression(NumberExpression(node))
  elif node of BoolExpression: discard
  elif node of StringExpression: discard
  elif node of CharExpression: discard
  elif node of NulExpression: discard
  elif node of TypeExpression: discard
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
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled expression"

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
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled statement"