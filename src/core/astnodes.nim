import types, tokens, errors

type
  # EXPRESSIONS

  Expression* = ref object of RootObj
    returnType*: Type
    token*: Token

  ErrorExpression* = ref object of Expression

  NumberExpression* = ref object of Expression

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

  DerefExpression* = ref object of Expression
    operand*: Expression

  CharExpression* = ref object of Expression

  ArrayExpression* = ref object of Expression
    values*: seq[Expression]

  IndexExpression* = ref object of Expression
    operand*: Expression
    index*: Expression

  NulExpression* = ref object of Expression

  TypeExpression* = ref object of Expression

  # STATEMENTS

  Statement* = ref object of RootObj

  DeclarationStatement* = ref object of Statement
    varType*: Type
    name*: Token
    value*: Expression

  BlockStatement* = ref object of Statement
    startToken*: Token
    endToken*: Token
    statements*: seq[Statement]

  AssignmentStatement* = ref object of Statement
    left*: Expression
    value*: Expression

  ErrorStatement* = ref object of Statement
    token*: Token

  BranchingStatement* = ref object of Statement
    condition*: Expression
    ifBlock*: BlockStatement
    elifBlocks*: seq[tuple[cond: Expression, elifBlock: BlockStatement]]
    elseBlock*: BlockStatement

  WhileStatement* = ref object of Statement
    token*: Token
    condition*: Expression
    whileBlock*: BlockStatement

  BreakStatement* = ref object of Statement
    token*: Token

  ContinueStatement* = ref object of Statement
    token*: Token

  # SPECIALS

  SpecialExprKind* = enum
    skExprError
    skNew, skArr, skLen

  SpecialExpression* = ref object of Expression
    kind*: SpecialExprKind
    args*: seq[Expression]

  SpecialStmtKind* = enum
    skStmtError
    skOut, skFree

  SpecialStatement* = ref object of Statement
    token*: Token
    kind*: SpecialStmtKind
    args*: seq[Expression]

# SPECIALS

proc newSpecialExpression*(token: Token, kind: SpecialExprKind, args: seq[Expression]): SpecialExpression =
  SpecialExpression(token: token, kind: kind, args: args, returnType: getUndefinedType())

proc newSpecialStatement*(token: Token, kind: SpecialStmtKind, args: seq[Expression]): SpecialStatement =
  SpecialStatement(token: token, kind: kind, args: args)

proc getSpecialExprKind*(token: Token): SpecialExprKind =
  case token.lexeme
  of "new": skNew
  of "arr": skArr
  of "len": skLen
  else:
    newError(errSpecial, token)
    return skExprError

proc getSpecialStmtKind*(token: Token): SpecialStmtKind =
  case token.lexeme
  of "out": skOut
  of "free": skFree
  else:
    newError(errSpecial, token)
    return skStmtError

# EXPRESSIONS

proc newTypeExpression*(token: Token, returnType: Type): TypeExpression =
  TypeExpression(token: token, returnType: returnType)

proc newNulExpression*(token: Token): NulExpression {.inline.} =
  NulExpression(token: token, returnType: getNulType())

proc newIndexExpression*(token: Token, operand: Expression, index: Expression): IndexExpression {.inline.} =
  IndexExpression(token: token, operand: operand, index: index, returnType: getUndefinedType())

proc newArrayExpression*(token: Token): ArrayExpression {.inline.} =
  ArrayExpression(token: token, returnType: getUndefinedType())

proc addExpr*(self: var ArrayExpression, expr: Expression) {.inline.} =
  self.values.add(expr)

proc newCharExpression*(token: Token): CharExpression {.inline.} =
  CharExpression(token: token, returnType: getCharType())

proc newDerefExpression*(token: Token, operand: Expression): DerefExpression {.inline.} =
  DerefExpression(token: token, operand: operand, returnType: getUndefinedType())

proc newCastExpression*(castToken: Token, castType: Type, value: Expression): CastExpression {.inline.} =
  CastExpression(token: castToken, returnType: castType, value: value)

proc newErrorExpression*(token: Token): ErrorExpression {.inline.} =
  ErrorExpression(token: token, returnType: getUndefinedType())

proc newNumberExpression*(value: Token): NumberExpression {.inline.} =
  NumberExpression(token: value, returnType: getInt64Type())

proc newBoolExpression*(value: Token): BoolExpression {.inline.} =
  BoolExpression(token: value, returnType: getBoolType())

proc newBinaryExpression*(left: Expression, op: Token, right: Expression): BinaryExpression {.inline.} =
  BinaryExpression(left: left, token: op, right: right, returnType: getUndefinedType())

proc newUnaryExpression*(operand: Expression, op: Token): UnaryExpression {.inline.} =
  UnaryExpression(operand: operand, token: op, returnType: getUndefinedType())

proc newIdentifierExpression*(name: Token): IdentifierExpression {.inline.} =
  IdentifierExpression(token: name, returnType: getUndefinedType())

proc newStringExpression*(token: Token): StringExpression {.inline.} =
  StringExpression(token: token, returnType: getArrayType(getCharType()))

# STATEMENTS

proc newWhileStatement*(token: Token, condition: Expression, whileBlock: BlockStatement): WhileStatement {.inline.} =
  WhileStatement(token: token, condition: condition, whileBlock: whileBlock)

proc newBreakStatement*(token: Token): BreakStatement {.inline.} =
  BreakStatement(token: token)

proc newContinueStatement*(token: Token): ContinueStatement {.inline.} =
  ContinueStatement(token: token)

proc newBranchingStatement*(condition: Expression, ifBlock: BlockStatement): BranchingStatement {.inline.} =
  BranchingStatement(condition: condition, ifBlock: ifBlock, elifBlocks: @[], elseBlock: nil)

proc addElif*(self: var BranchingStatement, condition: Expression, elifBlock: BlockStatement) {.inline.} =
  self.elifBlocks.add((condition, elifBlock))

proc setElse*(self: var BranchingStatement, elseBlock: BlockStatement) {.inline.} =
  self.elseBlock = elseBlock

proc newAssignmentStatement*(left: Expression, value: Expression): AssignmentStatement {.inline.} =
  AssignmentStatement(left: left, value: value)

proc newErrorStatement*(token: Token): ErrorStatement {.inline.} =
  ErrorStatement(token: token)

proc newBlockStatement*(startToken: Token, endToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: endToken, statements: @[])

proc newBlockStatement*(startToken: Token): BlockStatement {.inline.} =
  BlockStatement(startToken: startToken, endToken: tkInvalid.newToken(startToken.lexeme, 
    startToken.file, startToken.line, startToken.column, startToken.offset), statements: @[])

proc addStatement*(blockStmt: BlockStatement, stmt: Statement) {.inline.} =
  blockStmt.statements.add(stmt)

proc newDeclarationStatement*(
    varType: Type, name: Token, value: Expression
  ): DeclarationStatement {.inline.} =
  DeclarationStatement(name: name, value: value, varType: varType)