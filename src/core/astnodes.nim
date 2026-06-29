import types, tokens

type
  # EXPRESSIONS

  Expression* = ref object of RootObj
    returnType*: ptr Type

  ErrorExpression* = ref object of Expression
    token*: Token

  IntExpression* = ref object of Expression
    value*: Token

  BoolExpression* = ref object of Expression
    value*: Token

  BinaryExpression* = ref object of Expression
    left*: Expression
    op*: Token
    right*: Expression

  UnaryExpression* = ref object of Expression
    operand*: Expression
    op*: Token

  IdentifierExpression* = ref object of Expression
    name*: Token

  CastExpression* = ref object of Expression
    castToken*: Token
    value*: Expression

  # STATEMENTS

  Statement* = ref object of RootObj

  DeclarationStatement* = ref object of Statement
    varType*: ptr Type
    name*: Token
    value*: Expression

  BlockStatement* = ref object of Statement
    startToken*: Token
    endToken*: Token
    statements*: seq[Statement]

  AssignmentStatement* = ref object of Statement
    name*: Token
    value*: Expression

  ErrorStatement* = ref object of Statement
    token*: Token

  OutStatement* = ref object of Statement
    value*: Expression

#STATEMENTS

proc newOutStatement*(value: Expression): OutStatement {.inline.} =
  OutStatement(value: value)

proc newAssignmentStatement*(name: Token, value: Expression): AssignmentStatement {.inline.} =
  AssignmentStatement(name: name, value: value)

proc newErrorStatement*(token: Token): ErrorStatement {.inline.} =
  ErrorStatement(token: token)

proc newBlockStatement*(startToken: Token, endToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: endToken, statements: @[])

proc addStatement*(blockStmt: BlockStatement, stmt: Statement) {.inline.} =
  blockStmt.statements.add(stmt)

proc newDeclarationStatement*(
    varType: ptr Type, name: Token, value: Expression
  ): DeclarationStatement {.inline.} =
  DeclarationStatement(name: name, value: value, varType: varType)

# EXPRESSIONS

proc newCastExpression*(castToken: Token, castType: ptr Type, value: Expression): CastExpression {.inline.} =
  CastExpression(castToken: castToken, returnType: castType, value: value)

proc newErrorExpression*(token: Token): ErrorExpression {.inline.} =
  ErrorExpression(token: token, returnType: getUndefinedType())

proc newIntExpression*(value: Token): IntExpression {.inline.} =
  IntExpression(value: value, returnType: getIntType())

proc newBoolExpression*(value: Token): BoolExpression {.inline.} =
  BoolExpression(value: value, returnType: getBoolType())

proc newBinaryExpression*(left: Expression, op: Token, right: Expression): BinaryExpression {.inline.} =
  BinaryExpression(left: left, op: op, right: right, returnType: getUndefinedType())

proc newUnaryExpression*(operand: Expression, op: Token): UnaryExpression {.inline.} =
  UnaryExpression(operand: operand, op: op, returnType: getUndefinedType())

proc newIdentifierExpression*(name: Token): IdentifierExpression {.inline.} =
  IdentifierExpression(name: name, returnType: getUndefinedType())
