import types, tokens

type
  # EXPRESSIONS

  Expression* = ref object of RootObj
    returnType*: ptr Type
    token*: Token

  ErrorExpression* = ref object of Expression

  IntExpression* = ref object of Expression

  BoolExpression* = ref object of Expression

  BinaryExpression* = ref object of Expression
    left*: Expression
    right*: Expression

  UnaryExpression* = ref object of Expression
    operand*: Expression

  IdentifierExpression* = ref object of Expression

  CastExpression* = ref object of Expression
    value*: Expression

  StringExpression* = ref object of Expression

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
    values*: seq[Expression] = @[]

  BranchingStatement* = ref object of Statement
    condition*: Expression
    ifBlock*: BlockStatement
    elifBlocks*: seq[tuple[cond: Expression, elifBlock: BlockStatement]]
    elseBlock*: BlockStatement

#STATEMENTS

proc newBranchingStatement*(condition: Expression, ifBlock: BlockStatement): BranchingStatement {.inline.} =
  BranchingStatement(condition: condition, ifBlock: ifBlock, elifBlocks: @[], elseBlock: nil)

proc addElif*(self: var BranchingStatement, condition: Expression, elifBlock: BlockStatement) {.inline.} =
  self.elifBlocks.add((condition, elifBlock))

proc setElse*(self: var BranchingStatement, elseBlock: BlockStatement) {.inline.} =
  self.elseBlock = elseBlock

proc newOutStatement*(): OutStatement {.inline.} =
  OutStatement()

proc addExpr*(self: var OutStatement, expr: Expression) {.inline.} =
  self.values.add(expr)

proc newAssignmentStatement*(name: Token, value: Expression): AssignmentStatement {.inline.} =
  AssignmentStatement(name: name, value: value)

proc newErrorStatement*(token: Token): ErrorStatement {.inline.} =
  ErrorStatement(token: token)

proc newBlockStatement*(startToken: Token, endToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: endToken, statements: @[])

proc newBlockStatement*(startToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: tkInvalid.newToken("", "", 1, 1, 0), statements: @[])

proc addStatement*(blockStmt: BlockStatement, stmt: Statement) {.inline.} =
  blockStmt.statements.add(stmt)

proc newDeclarationStatement*(
    varType: ptr Type, name: Token, value: Expression
  ): DeclarationStatement {.inline.} =
  DeclarationStatement(name: name, value: value, varType: varType)

# EXPRESSIONS

proc newStringExpression*(value: Token): StringExpression {.inline.} =
  StringExpression(token: value, returnType: getStringType())

proc newCastExpression*(castToken: Token, castType: ptr Type, value: Expression): CastExpression {.inline.} =
  CastExpression(token: castToken, returnType: castType, value: value)

proc newErrorExpression*(token: Token): ErrorExpression {.inline.} =
  ErrorExpression(token: token, returnType: getUndefinedType())

proc newIntExpression*(value: Token): IntExpression {.inline.} =
  IntExpression(token: value, returnType: getIntType())

proc newBoolExpression*(value: Token): BoolExpression {.inline.} =
  BoolExpression(token: value, returnType: getBoolType())

proc newBinaryExpression*(left: Expression, op: Token, right: Expression): BinaryExpression {.inline.} =
  BinaryExpression(left: left, token: op, right: right, returnType: getUndefinedType())

proc newUnaryExpression*(operand: Expression, op: Token): UnaryExpression {.inline.} =
  UnaryExpression(operand: operand, token: op, returnType: getUndefinedType())

proc newIdentifierExpression*(name: Token): IdentifierExpression {.inline.} =
  IdentifierExpression(token: name, returnType: getUndefinedType())
