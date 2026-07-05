import ../[astnodes, types, tokens]
import visitor

type
  ASTPrinterVisitor* = ref object of Visitor
    output*: string

proc newASTPrinterVisitor*(): ASTPrinterVisitor =
  ASTPrinterVisitor(output: "")

method visitExpression*(visitor: ASTPrinterVisitor, node: Expression) {.base.}

method visitErrorExpression*(visitor: ASTPrinterVisitor, node: ErrorExpression): auto =
  visitor.output.add("ErrorExpression(" & node.token.mean & ")")

method visitIntExpression*(visitor: ASTPrinterVisitor, node: IntExpression): auto =
  visitor.output.add("IntExpression(" & $node.token.lexeme & ")")

method visitBoolExpression*(visitor: ASTPrinterVisitor, node: BoolExpression): auto =
  visitor.output.add("BoolExpression(" & $node.token.lexeme & ")")

method visitBinaryExpression*(visitor: ASTPrinterVisitor, node: BinaryExpression): auto =
  visitor.output.add("BinaryExpression(")
  visitor.visitExpression(node.left)
  visitor.output.add(", " & node.token.lexeme & ", ")
  visitor.visitExpression(node.right)
  visitor.output.add(")")

method visitUnaryExpression*(visitor: ASTPrinterVisitor, node: UnaryExpression): auto =
  visitor.output.add("UnaryExpression(" & node.token.lexeme & ", ")
  visitor.visitExpression(node.operand)
  visitor.output.add(")")

method visitIdentifierExpression*(visitor: ASTPrinterVisitor, node: IdentifierExpression): auto =
  visitor.output.add("IdentifierExpression(" & node.token.lexeme & ")")

method visitCastExpression*(visitor: ASTPrinterVisitor, node: CastExpression): auto =
  visitor.output.add("CastExpression(")
  visitor.visitExpression(node.value)
  visitor.output.add(", ")
  visitor.output.add($node.returnType & ")")

method visitStringExpression*(visitor: ASTPrinterVisitor, node: StringExpression): auto =
  visitor.output.add("StringExpression(\"" & node.token.lexeme & "\")")

method visitNewExpression*(visitor: ASTPrinterVisitor, node: NewExpression): auto =
  visitor.output.add("NewExpression(")
  visitor.visitExpression(node.value)
  visitor.output.add(")")

method visitDerefExpression*(visitor: ASTPrinterVisitor, node: DerefExpression): auto =
  visitor.output.add("DerefExpression(")
  visitor.visitExpression(node.operand)
  visitor.output.add(")")

method visitCharExpression*(visitor: ASTPrinterVisitor, node: CharExpression): auto =
  visitor.output.add("CharExpression('" & node.token.lexeme & "')")

method visitDeclarationStatement*(visitor: ASTPrinterVisitor, node: DeclarationStatement): auto =
  visitor.output.add("DeclarationStatement(" & $node.varType & ", ")
  visitor.output.add(node.name.lexeme & ", ")
  visitor.visitExpression(node.value)
  visitor.output.add(")")

method visitAssignmentStatement*(visitor: ASTPrinterVisitor, node: AssignmentStatement): auto =
  visitor.output.add("AssignmentStatement(")
  visitor.visitExpression(node.left)
  visitor.output.add(", ")
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
  visitor.output.add("ErrorStatement(" & node.token.mean & ")")

method visitOutStatement*(visitor: ASTPrinterVisitor, node: OutStatement): auto =
  visitor.output.add("OutStatement(")
  for i, expr in node.values:
    if i > 0:
      visitor.output.add(", ")
    visitor.visitExpression(expr)
  visitor.output.add(")")

method visitBranchingStatement*(visitor: ASTPrinterVisitor, node: BranchingStatement): auto =
  visitor.output.add("BranchingStatement(")
  visitor.visitExpression(node.condition)
  visitor.output.add(", ")
  visitor.visitStatement(node.ifBlock)
  
  for el in node.elifBlocks:
    visitor.output.add(", elif(")
    visitor.visitExpression(el.cond)
    visitor.output.add(", ")
    visitor.visitStatement(el.elifBlock)
    visitor.output.add(")")
  
  if node.elseBlock != nil:
    visitor.output.add(", else(")
    visitor.visitStatement(node.elseBlock)
    visitor.output.add(")")
  
  visitor.output.add(")")

method visitFreeStatement*(visitor: ASTPrinterVisitor, node: FreeStatement): auto =
  visitor.output.add("FreeStatement(")
  visitor.visitExpression(node.expr)
  visitor.output.add(")")

method visitExpression*(visitor: ASTPrinterVisitor, node: Expression) =
  if node of ErrorExpression:
    visitor.visitErrorExpression(ErrorExpression(node))
  elif node of IntExpression:
    visitor.visitIntExpression(IntExpression(node))
  elif node of BoolExpression:
    visitor.visitBoolExpression(BoolExpression(node))
  elif node of BinaryExpression:
    visitor.visitBinaryExpression(BinaryExpression(node))
  elif node of UnaryExpression:
    visitor.visitUnaryExpression(UnaryExpression(node))
  elif node of IdentifierExpression:
    visitor.visitIdentifierExpression(IdentifierExpression(node))
  elif node of CastExpression:
    visitor.visitCastExpression(CastExpression(node))
  elif node of StringExpression:
    visitor.visitStringExpression(StringExpression(node))
  elif node of NewExpression:
    visitor.visitNewExpression(NewExpression(node))
  elif node of DerefExpression:
    visitor.visitDerefExpression(DerefExpression(node))
  elif node of CharExpression:
    visitor.visitCharExpression(CharExpression(node))
  else:
    echo "[ASTPrinterVisitor] WARNING: unhandled expression"
    visitor.output.add("!ASTPrinterVisitor.UNHANDLED_EXPRESSION!")

method visitStatement*(visitor: ASTPrinterVisitor, node: Statement) =
  if node of DeclarationStatement:
    visitor.visitDeclarationStatement(DeclarationStatement(node))
  elif node of BlockStatement:
    visitor.visitBlockStatement(BlockStatement(node))
  elif node of ErrorStatement:
    visitor.visitErrorStatement(ErrorStatement(node))
  elif node of AssignmentStatement:
    visitor.visitAssignmentStatement(AssignmentStatement(node))
  elif node of OutStatement:
    visitor.visitOutStatement(OutStatement(node))
  elif node of BranchingStatement:
    visitor.visitBranchingStatement(BranchingStatement(node))
  elif node of FreeStatement:
    visitor.visitFreeStatement(FreeStatement(node))
  else:
    echo "[ASTPrinterVisitor] WARNING: unhandled statement"
    visitor.output.add("!ASTPrinterVisitor.UNHANDLED_STATEMENT!")

proc printStatement*(visitor: ASTPrinterVisitor, node: Statement): string =
  visitor.output = ""
  visitor.visitStatement(node)
  return visitor.output
