import ../astnodes
import visitor

type
  ASTPrinterVisitor* = ref object of Visitor
    output*: string

proc newASTPrinterVisitor*(): ASTPrinterVisitor =
  ASTPrinterVisitor(output: "")

method visitErrorExpression*(visitor: ASTPrinterVisitor, node: ErrorExpression): auto =
  visitor.output.add("ErrorExpression(" & node.token.lexeme & ")")

method visitIntLitExpression*(visitor: ASTPrinterVisitor, node: IntLitExpression): auto =
  visitor.output.add("IntLitExpression(" & $node.value.lexeme & ")")

method visitExpression*(visitor: ASTPrinterVisitor, node: Expression) {.base.}

method visitBinaryExpression*(visitor: ASTPrinterVisitor, node: BinaryExpression): auto =
  visitor.output.add("BinaryExpression(")
  visitor.visitExpression(node.left)
  visitor.output.add(", " & node.op.lexeme & ", ")
  visitor.visitExpression(node.right)
  visitor.output.add(")")

method visitUnaryExpression*(visitor: ASTPrinterVisitor, node: UnaryExpression): auto =
  visitor.output.add("UnaryExpression(" & node.op.lexeme & ", ")
  visitor.visitExpression(node.operand)
  visitor.output.add(")")

method visitIdentifierExpression*(visitor: ASTPrinterVisitor, node: IdentifierExpression): auto =
  visitor.output.add("IdentifierExpression(" & node.name.lexeme & ")")

method visitDeclarationStatement*(visitor: ASTPrinterVisitor, node: DeclarationStatement): auto =
  visitor.output.add("DeclarationStatement(" & node.typeToken.lexeme & ", ")
  visitor.output.add(node.name.lexeme & ", ")
  visitor.visitExpression(node.value)
  visitor.output.add(")")

method visitAssignmentStatement*(visitor: ASTPrinterVisitor, node: AssignmentStatement): auto =
  visitor.output.add("AssignmentStatement(" & node.name.lexeme & ", ")
  visitor.visitExpression(node.value)
  visitor.output.add(")")

method visitStatement*(visitor: ASTPrinterVisitor, node: Statement) {.base.}

method visitBlockStatement*(visitor: ASTPrinterVisitor, node: BlockStatement): auto =
  visitor.output.add("BlockStatement([")
  for i, stmt in node.statements:
    if i > 0:
      visitor.output.add(", ")
    visitor.visitStatement(stmt)
  visitor.output.add("])")

method visitErrorStatement*(visitor: ASTPrinterVisitor, node: ErrorStatement): auto =
  visitor.output.add("ErrorStatement(" & node.token.lexeme & ")")

method visitExpression*(visitor: ASTPrinterVisitor, node: Expression) =
  if node of ErrorExpression:
    visitor.visitErrorExpression(ErrorExpression(node))
  elif node of IntLitExpression:
    visitor.visitIntLitExpression(IntLitExpression(node))
  elif node of BinaryExpression:
    visitor.visitBinaryExpression(BinaryExpression(node))
  elif node of UnaryExpression:
    visitor.visitUnaryExpression(UnaryExpression(node))
  elif node of IdentifierExpression:
    visitor.visitIdentifierExpression(IdentifierExpression(node))

method visitStatement*(visitor: ASTPrinterVisitor, node: Statement) =
  if node of DeclarationStatement:
    visitor.visitDeclarationStatement(DeclarationStatement(node))
  elif node of BlockStatement:
    visitor.visitBlockStatement(BlockStatement(node))
  elif node of ErrorStatement:
    visitor.visitErrorStatement(ErrorStatement(node))
  elif node of AssignmentStatement:
    visitor.visitAssignmentStatement(AssignmentStatement(node))

proc printStatement*(visitor: ASTPrinterVisitor, node: Statement): string =
  visitor.output = ""
  visitor.visitStatement(node)
  return visitor.output
