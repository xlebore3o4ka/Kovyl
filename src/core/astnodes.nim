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

  # STATEMENTS

  Statement* = ref object of RootObj

  VariableDeclarationStatement* = ref object of Statement
    typeToken*: Token
    varType*: ptr Type
    name*: Token
    value*: Expression

  BlockStatement* = ref object of Statement
    startToken*: Token
    endToken*: Token
    statements*: seq[Statement]

  ErrorStatement* = ref object of Statement
    token*: Token

proc newErrorStatement*(token: Token): ErrorStatement {.inline.} =
  ErrorStatement(token: token)

proc newBlockStatement*(startToken: Token, endToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: endToken, statements: @[])

proc addStatement*(blockStmt: BlockStatement, stmt: Statement) {.inline.} =
  blockStmt.statements.add(stmt)

proc newVariableDeclarationStatement*(
    typeToken: Token, name: Token, value: Expression
  ): VariableDeclarationStatement {.inline.} =
  VariableDeclarationStatement(typeToken: typeToken, name: name, value: value, varType: getUndefinedType())

# EXPRESSIONS

proc newErrorExpression*(token: Token): ErrorExpression {.inline.} =
  ErrorExpression(token: token, returnType: getUndefinedType())

proc newIntLitExpression*(value: Token): IntLitExpression {.inline.} =
  IntLitExpression(value: value, returnType: getIntType())

proc newBinaryExpression*(left: Expression, op: Token, right: Expression): BinaryExpression {.inline.} =
  BinaryExpression(left: left, op: op, right: right, returnType: getUndefinedType())

proc newUnaryExpression*(operand: Expression, op: Token): UnaryExpression {.inline.} =
  UnaryExpression(operand: operand, op: op, returnType: getUndefinedType())