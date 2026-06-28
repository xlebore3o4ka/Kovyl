import ../[astnodes, tokens, types, errors]
import visitor
import std/tables

type
  Symbol = object
    token: Token
    symbolType: ptr Type
    index: Natural

  SemanticAnalyzerVisitor* = ref object of Visitor
    symbolTable: OrderedTable[string, Symbol]

proc newSymbol(visitor: SemanticAnalyzerVisitor, token: Token, symbolType: ptr Type) =
  let symbol = Symbol(
    token: token,
    symbolType: symbolType,
    index: visitor.symbolTable.len
  )
  visitor.symbolTable[token.lexeme] = symbol

proc newSemanticAnalyzerVisitor*(): SemanticAnalyzerVisitor =
  SemanticAnalyzerVisitor()

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) {.base.}

method visitBinaryExpression*(visitor: SemanticAnalyzerVisitor, node: BinaryExpression): auto =
  visitor.visitExpression(node.left)
  visitor.visitExpression(node.right)

  case node.op.kind:
  of tkPlus, tkMinus, tkStar, tkSlash:
    if node.left.returnType == getIntType() and node.right.returnType == getIntType():
      node.returnType = getIntType()
    elif node.left.returnType == getUintType() and node.right.returnType == getUintType():
      node.returnType = getUintType()
  of tkGT, tkLT, tkEQ, tkNEQ, tkGTE, tkLTE:
    if node.left.returnType == getIntType() and node.right.returnType == getIntType():
      node.returnType = getBoolType()
    elif node.left.returnType == getUintType() and node.right.returnType == getUintType():
      node.returnType = getBoolType()
  of tkAnd, tkOr:
    if node.left.returnType == getBoolType() and node.right.returnType == getBoolType():
      node.returnType = getBoolType()
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled binary operator " & node.op.mean()

  if node.returnType == getUndefinedType():
    newError(errBinaryTypeMismatch, node.op, @{
        "@0": $node.op.lexeme, "@1": $node.left.returnType, "@2": $node.right.returnType})

method visitUnaryExpression*(visitor: SemanticAnalyzerVisitor, node: UnaryExpression): auto =
  visitor.visitExpression(node.operand)

  case node.op.kind:
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
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled unary operator " & node.op.mean()

  if node.returnType == getUndefinedType():
    newError(errUnaryTypeMismatch, node.op, @{
      "@0": $node.op.lexeme, "@1": $node.operand.returnType})

method visitIdentifierExpression*(visitor: SemanticAnalyzerVisitor, node: IdentifierExpression): auto =
  if node.name.lexeme notin visitor.symbolTable:
    newError(errUndeclaredSymbol, node.name, @{"@0": node.name.lexeme})
    return

  node.returnType = visitor.symbolTable[node.name.lexeme].symbolType

method visitCastExpression*(visitor: SemanticAnalyzerVisitor, node: CastExpression): auto =
  visitor.visitExpression(node.value)
  
  # Cast always succeeds (for now, with int/uint/bool types)
  # In a more complete implementation, you'd check if the cast is valid

method visitStatement*(visitor: SemanticAnalyzerVisitor, node: Statement) {.base.}

method visitDeclarationStatement*(visitor: SemanticAnalyzerVisitor, node: DeclarationStatement): auto =
  visitor.visitExpression(node.value)

  if node.name.lexeme in visitor.symbolTable:
    let name = visitor.symbolTable[node.name.lexeme].token
    newError(errRedeclaration, node.name, 
      @{"@0": node.name.lexeme, "@1": name.file, "@2": $name.line, "@3": $name.column}
    )
    return

  if node.varType != node.value.returnType:
    newError(errTypeMismatch, node.name, @{"@0": $node.varType, "@1": $node.value.returnType})
    return

  visitor.newSymbol(node.name, node.varType)

method visitBlockStatement*(visitor: SemanticAnalyzerVisitor, node: BlockStatement): auto =
  # TODO: push scope
  for stmt in node.statements:
    visitor.visitStatement(stmt)
  # TODO: pop scope

method visitAssignmentStatement*(visitor: SemanticAnalyzerVisitor, node: AssignmentStatement): auto =
  visitor.visitExpression(node.value)

  if node.name.lexeme notin visitor.symbolTable:
    newError(errUndeclaredSymbol, node.name, @{"@0": node.name.lexeme})
    return

  let varType = visitor.symbolTable[node.name.lexeme].symbolType
  if varType != node.value.returnType:
    newError(errTypeMismatch, node.name, @{"@0": $varType, "@1": $node.value.returnType})
    return

method visitExpression*(visitor: SemanticAnalyzerVisitor, node: Expression) =
  if node of ErrorExpression: discard
  elif node of IntExpression: discard
  elif node of BoolExpression: discard
  elif node of BinaryExpression:
    visitor.visitBinaryExpression(BinaryExpression(node))
  elif node of UnaryExpression:
    visitor.visitUnaryExpression(UnaryExpression(node))
  elif node of IdentifierExpression:
    visitor.visitIdentifierExpression(IdentifierExpression(node))
  elif node of CastExpression:
    visitor.visitCastExpression(CastExpression(node))
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
  else:
    echo "[SemanticAnalyzerVisitor] WARNING: unhandled statement"