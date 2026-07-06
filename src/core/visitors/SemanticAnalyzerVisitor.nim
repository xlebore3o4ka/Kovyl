import ../[astnodes, tokens, types, errors]
import visitor
import std/tables

type
  Symbol = object
    token: Token
    symbolType: Type
    index: Natural

  SemanticAnalyzerVisitor* = ref object of Visitor
    symbolTable: Table[string, Natural]
    scopedSymbolPool: seq[Symbol]
    symbolIndex: Natural = 0
    scopeStack: seq[Natural]

proc newSymbol(visitor: SemanticAnalyzerVisitor, token: Token, symbolType: Type) =
  let symbol = Symbol(
    token: token,
    symbolType: symbolType,
    index: visitor.symbolTable.len
  )
  let index = visitor.symbolIndex
  visitor.symbolTable[token.lexeme] = index
  visitor.symbolIndex.inc
  visitor.scopedSymbolPool.setLen(visitor.symbolIndex)
  visitor.scopedSymbolPool[index] = symbol

proc getSymbol(visitor: SemanticAnalyzerVisitor, name: string): Symbol {.inline.} =
  return visitor.scopedSymbolPool[visitor.symbolTable[name]]

proc pushScope(visitor: SemanticAnalyzerVisitor) {.inline.} =
  visitor.scopeStack.add(visitor.symbolIndex)

proc popScope(visitor: SemanticAnalyzerVisitor) {.inline.} =
  visitor.symbolIndex = visitor.scopeStack.pop()
  for symbol in visitor.scopedSymbolPool[visitor.symbolIndex..^1]:
    visitor.symbolTable.del(symbol.token.lexeme)

proc newSemanticAnalyzerVisitor*(): SemanticAnalyzerVisitor =
  SemanticAnalyzerVisitor()

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) {.base.}

method visitBinaryExpression*(visitor: SemanticAnalyzerVisitor, node: BinaryExpression): auto =
  visitor.visitExpression(node.left)
  visitor.visitExpression(node.right)

  case node.token.kind:
  of tkPlus, tkMinus, tkStar, tkSlash:
    if node.left.returnType == getIntType() and node.right.returnType == getIntType():
      node.returnType = getIntType()
      
    elif node.left.returnType == getUintType() and node.right.returnType == getUintType():
      node.returnType = getUintType()

  of tkGT, tkLT, tkGTE, tkLTE:
    if node.left.returnType == getIntType() and node.right.returnType == getIntType():
      node.returnType = getBoolType()

    elif node.left.returnType == getUintType() and node.right.returnType == getUintType():
      node.returnType = getBoolType()

  of tkEQ, tkNEQ:
    if node.left.returnType == getIntType() and node.right.returnType == getIntType():
      node.returnType = getBoolType()

    elif node.left.returnType == getUintType() and node.right.returnType == getUintType():
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
    if node.operand.returnType == getIntType():
      node.returnType = getIntType()
  of tkPlus:
    if node.operand.returnType == getUintType():
      node.returnType = getUintType()
    elif node.operand.returnType == getIntType():
      node.returnType = getIntType()
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
    node.returnType = getArrayType(getUndefinedType())
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
  
  node.returnType = getArrayType(firstType)

method visitIndexExpression*(visitor: SemanticAnalyzerVisitor, node: IndexExpression): auto =
  visitor.visitExpression(node.operand)
  visitor.visitExpression(node.index)
  
  if node.index.returnType != getIntType():
    newError(errTypeMismatch, node.index.token, @{"@0": "int", "@1": $node.index.returnType})
    return
  
  if node.operand.returnType.kind != typeArray:
    newError(errTypeMismatch, node.operand.token, @{"@0": "array", "@1": $node.operand.returnType})
    return
  
  node.returnType = node.operand.returnType.arrayBaseType

# STATEMENTS

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) {.base.}

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  visitor.visitExpression(node.value)

  var varType = node.varType

  if node.name.lexeme in visitor.symbolTable:
    let name = visitor.getSymbol(node.name.lexeme).token
    newError(errRedeclaration, node.name, 
      @{"@0": node.name.lexeme, "@1": name.file, "@2": $name.line, "@3": $name.column}
    )
    varType = getUndefinedType()

  if node.varType != node.value.returnType and not (
      node.varType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
    ):
    newError(errTypeMismatch, node.name, @{"@0": $node.varType, "@1": $node.value.returnType})
    varType = getUndefinedType()

  visitor.newSymbol(node.name, varType)

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  for stmt in node.statements:
    visitor.visitStatement(stmt)

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  visitor.visitExpression(node.value)
  
  if node.left of IdentifierExpression:
    let name = IdentifierExpression(node.left).token
    if name.lexeme notin visitor.symbolTable:
      newError(errUndeclaredSymbol, name, @{"@0": name.lexeme})
      return
    
    let varType = visitor.getSymbol(name.lexeme).symbolType
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
      
    if ptrType.ptrBaseType != node.value.returnType and not (
        ptrType.ptrBaseType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
      ):
      newError(errTypeMismatch, node.left.token, @{"@0": $ptrType.ptrBaseType, "@1": $node.value.returnType})
      return

  elif node.left of IndexExpression:
    let indexExpr = IndexExpression(node.left)
    visitor.visitExpression(indexExpr.operand)
    visitor.visitExpression(indexExpr.index)
    
    if indexExpr.operand.returnType.kind != typeArray:
      newError(errTypeMismatch, node.left.token, @{"@0": "array", "@1": $indexExpr.operand.returnType})
      return
    
    if indexExpr.index.returnType != getIntType():
      newError(errTypeMismatch, indexExpr.index.token, @{"@0": "int", "@1": $indexExpr.index.returnType})
      return
    
    let elemType = indexExpr.operand.returnType.arrayBaseType
    if elemType != node.value.returnType and not (
        elemType.kind in {typePtr, typeArray} and node.value.returnType == getNulType()
      ):
      newError(errTypeMismatch, node.left.token, @{"@0": $elemType, "@1": $node.value.returnType})
      return

method visitBranchingStatement*(visitor: SemanticAnalyzerVisitor, node: BranchingStatement): auto =
  visitor.pushScope()
  visitor.visitExpression(node.condition)
  
  if node.condition.returnType != getBoolType():
    newError(errTypeMismatch, node.condition.token, @{"@0": "bool", "@1": $node.condition.returnType})
  
  visitor.visitStatement(node.ifBlock)
  visitor.popScope()
  
  for el in node.elifBlocks:
    visitor.pushScope()
    visitor.visitExpression(el.cond)
    
    if el.cond.returnType != getBoolType():
      newError(errTypeMismatch, el.cond.token, @{"@0": "bool", "@1": $el.cond.returnType})
    
    visitor.visitStatement(el.elifBlock)
    visitor.popScope()
  
  if node.elseBlock != nil:
    visitor.pushScope()
    visitor.visitStatement(node.elseBlock)
    visitor.popScope()

# SPECIALS

proc expectArgsLen(self: SpecialExpression | SpecialStatement, len: int) {.inline.} =
  if self.args.len != len:
    newError(errArgumentCount, self.token, @{"@0": $self.token.lexeme, "@1": $len, "@2": $self.args.len})

proc get(self: SpecialExpression | SpecialStatement, idx: int): Expression {.inline.} =
  return self.args[idx]

proc getTyped(self: SpecialExpression | SpecialStatement, idx: int, 
    expectedType: TypeKind): Expression {.inline.} =
  result = self.args[idx]
  if result.returnType.kind != expectedType:
    newError(errTypeMismatch, result.token, @{"@0": $expectedType, "@1": $result.returnType})
    result = newErrorExpression(result.token)

proc getTypedAny(self: SpecialExpression | SpecialStatement, idx: int, 
    expectedTypes: set[TypeKind]): Expression {.inline.} =
  result = self.args[idx]
  if result.returnType.kind notin expectedTypes:
    newError(errTypeMismatch, result.token, @{"@0": $expectedTypes, "@1": $result.returnType})
    result = newErrorExpression(result.token)

method visitSpecialExpression*(visitor: SemanticAnalyzerVisitor, node: SpecialExpression): auto =
  for arg in node.args:
    visitor.visitExpression(arg)

  case node.kind:
  of skNew: 
    node.expectArgsLen(1)
    node.returnType = getPtrType(node.get(0).returnType)

  of skArr: 
    node.expectArgsLen(2)

    let arrayBaseType = node.get(0)
    if not (arrayBaseType of TypeExpression):
      newError(errTypeMismatch, arrayBaseType.token, @{"@0": "type", "@1": $arrayBaseType.returnType})
      return

    let size = node.getTyped(1, typeUint)
    if size of ErrorExpression: return

    node.returnType = getArrayType(arrayBaseType.returnType)

  of skLen:
    node.expectArgsLen(1)

    let arr = node.getTyped(0, typeArray)
    if arr of ErrorExpression: return

    node.returnType = getUintType()
  else: 
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled special expression"

method visitSpecialStatement*(visitor: SemanticAnalyzerVisitor, node: SpecialStatement): auto =
  for arg in node.args:
    visitor.visitExpression(arg)
  
  case node.kind:
  of skOut:
    for arg in node.args:
      if arg.returnType == getArrayType(getCharType()):
        continue
      elif arg.returnType.kind in {typePtr, typeArray, typeNul}:
        newError(errProhibitedType, arg.token, @{"@0": $arg.returnType})
  
  of skFree:
    node.expectArgsLen(1)
    let arg = node.getTypedAny(0, {typePtr, typeArray})
    if arg of ErrorExpression: return
  
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled special statement"

# GENERAL

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) =
  if node of ErrorExpression: discard
  elif node of IntExpression: discard
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
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled statement"