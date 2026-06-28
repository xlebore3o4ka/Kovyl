import ../astnodes

type
  Visitor* = ref object of RootObj

method visitErrorExpression*(visitor: Visitor, node: ErrorExpression): auto {.base.} =
  discard

method visitIntLitExpression*(visitor: Visitor, node: IntLitExpression): auto {.base.} =
  discard

method visitBinaryExpression*(visitor: Visitor, node: BinaryExpression): auto {.base.} =
  discard

method visitUnaryExpression*(visitor: Visitor, node: UnaryExpression): auto {.base.} =
  discard

method visitIdentifierExpression*(visitor: Visitor, node: IdentifierExpression): auto {.base.} =
  discard

method visitCastExpression*(visitor: Visitor, node: CastExpression): auto {.base.} =
  discard

method visitDeclarationStatement*(visitor: Visitor, node: DeclarationStatement): auto {.base.} =
  discard

method visitBlockStatement*(visitor: Visitor, node: BlockStatement): auto {.base.} =
  discard

method visitErrorStatement*(visitor: Visitor, node: ErrorStatement): auto {.base.} =
  discard

method visitAssignmentStatement*(visitor: Visitor, node: AssignmentStatement): auto {.base.} =
  discard