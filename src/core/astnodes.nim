import types, tokens

type
  Expression* = ref object of RootObj
    returnType*: ptr Type

  ErrorExpression* = ref object of Expression
    token*: Token

  IntLitExpression* = ref object of Expression
    value*: Token

  BinaryExpression* = ref object of Expression
    left*: Expression
    op*: Token
    right*: Expression

  UnaryExpression* = ref object of Expression
    operand*: Expression
    op*: Token

proc newErrorExpression*(token: Token): ErrorExpression {.inline.} =
  ErrorExpression(token: token, returnType: getUndefinedType())

proc newIntLitExpression*(value: Token): IntLitExpression {.inline.} =
  IntLitExpression(value: value, returnType: getIntType())

proc newBinaryExpression*(left: Expression, op: Token, right: Expression): BinaryExpression {.inline.} =
  BinaryExpression(left: left, op: op, right: right, returnType: getUndefinedType())

proc newUnaryExpression*(operand: Expression, op: Token): UnaryExpression {.inline.} =
  UnaryExpression(operand: operand, op: op, returnType: getUndefinedType())