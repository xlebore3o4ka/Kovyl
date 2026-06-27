import astnodes, visitors/visitor

# EXPRESSION

method accept*(node: ErrorExpression, visitor: Visitor): auto {.base.} =
  visitor.visitErrorExpression(node)

method accept*(node: IntLitExpression, visitor: Visitor): auto {.base.} =
  visitor.visitIntLitExpression(node)

method accept*(node: BinaryExpression, visitor: Visitor): auto {.base.} =
  visitor.visitBinaryExpression(node)

method accept*(node: UnaryExpression, visitor: Visitor): auto {.base.} =
  visitor.visitUnaryExpression(node)

# STATEMENTS

method accept*(node: VariableDeclarationStatement, visitor: Visitor): auto {.base.} =
  visitor.visitVariableDeclarationStatement(node)

method accept*(node: BlockStatement, visitor: Visitor): auto {.base.} =
  visitor.visitBlockStatement(node)

method accept*(node: ErrorStatement, visitor: Visitor): auto {.base.} =
  visitor.visitErrorStatement(node)