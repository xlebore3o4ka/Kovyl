import ../astnodes

type
  Visitor* = ref object of RootObj

method visitErrorExpression*(visitor: Visitor, node: ErrorExpression): auto {.base.} =
  discard

method visitIntExpression*(visitor: Visitor, node: IntExpression): auto {.base.} =
  discard

method visitBoolExpression*(visitor: Visitor, node: BoolExpression): auto {.base.} =
  discard

method visitBinaryExpression*(visitor: Visitor, node: BinaryExpression): auto {.base.} =
  discard

method visitUnaryExpression*(visitor: Visitor, node: UnaryExpression): auto {.base.} =
  discard

method visitIdentifierExpression*(visitor: Visitor, node: IdentifierExpression): auto {.base.} =
  discard

method visitCastExpression*(visitor: Visitor, node: CastExpression): auto {.base.} =
  discard

method visitStringExpression*(visitor: Visitor, node: StringExpression): auto {.base.} =
  discard

method visitNewExpression*(visitor: Visitor, node: NewExpression): auto {.base.} =
  discard

method visitDerefExpression*(visitor: Visitor, node: DerefExpression): auto {.base.} =
  discard

method visitCharExpression*(visitor: Visitor, node: CharExpression): auto {.base.} =
  discard

method visitDeclarationStatement*(visitor: Visitor, node: DeclarationStatement): auto {.base.} =
  discard

method visitBlockStatement*(visitor: Visitor, node: BlockStatement): auto {.base.} =
  discard

method visitErrorStatement*(visitor: Visitor, node: ErrorStatement): auto {.base.} =
  discard

method visitAssignmentStatement*(visitor: Visitor, node: AssignmentStatement): auto {.base.} =
  discard

method visitOutStatement*(visitor: Visitor, node: OutStatement): auto {.base.} =
  discard

method visitBranchingStatement*(visitor: Visitor, node: BranchingStatement): auto {.base.} =
  discard

method visitFreeStatement*(visitor: Visitor, node: FreeStatement): auto {.base.} =
  discard