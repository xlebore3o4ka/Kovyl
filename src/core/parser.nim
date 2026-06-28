import lexer, astnodes, tokens, errors, types

type Parser* = object
  file: string
  lexer: Lexer
  
proc newParser*(text, file: string): Parser =
  Parser(file: file, lexer: newLexer(text, file))

proc newError(self: Parser,
  kind: ErrorKind, token: Token, 
  args: seq[(string, string)] = @[]) =
  if not self.lexer.hasError:
    newError(kind, token, args)

proc expectToken*(self: var Parser, expected: TokenKind): Token =
  let token = self.lexer.nextToken()
  if token.kind != expected:
    self.newError(errExpectedSyntax, token, @{"@0": expected.mean(), "@1": token.mean()})
    return tkInvalid.newToken(token.lexeme, self.file, token.line, token.column, token.offset)
  return token

proc parseExpr(self: var Parser): Expression

proc parsePrimary(self: var Parser): Expression =
  let token = self.lexer.nextToken()

  if token.kind == tkLParen:
    result = self.parseExpr()
    discard self.expectToken(tkRParen)
    return result

  elif token.kind == tkIntLiteral:
    return newIntLitExpression(token)

  elif token.kind == tkIdentifier:
    return newIdentifierExpression(token)

  self.newError(errExpression, token, @{"@0": token.mean()})
  return newErrorExpression(token)

proc parseUnary(self: var Parser): Expression =
  if self.lexer.peekToken().kind in {tkPlus, tkMinus}:
    let token = self.lexer.nextToken()
    return newUnaryExpression(self.parseUnary(), token)

  return self.parsePrimary()

proc parseType(self: var Parser): ptr Type =
  let token = self.lexer.nextToken()
  case token.kind:
  of tkInt: return getIntType()
  of tkUint: return getUintType()
  else: 
    self.newError(errSyntax, token)
    return getUndefinedType()

proc parseCast(self: var Parser): Expression =
  var expression = self.parseUnary()
  
  if self.lexer.peekToken().kind == tkColon:
    let colonToken = self.lexer.nextToken()
    let castType = self.parseType()
    return newCastExpression(colonToken, castType, expression)
  
  return expression

proc parseMulDiv(self: var Parser): Expression =
  var expression = self.parseCast()

  while self.lexer.peekToken().kind in {tkStar, tkSlash}:
    let op = self.lexer.nextToken()
    let right = self.parseCast()
    expression = newBinaryExpression(expression, op, right)

  return expression

proc parseAddSub(self: var Parser): Expression =
  var expression = self.parseMulDiv()

  while self.lexer.peekToken().kind in {tkPlus, tkMinus}:
    let op = self.lexer.nextToken()
    let right = self.parseMulDiv()
    expression = newBinaryExpression(expression, op, right)

  return expression

proc parseExpr(self: var Parser): Expression =
  return self.parseAddSub()

proc parseVariableDecl(self: var Parser): DeclarationStatement {.inline.} =
  var varType = self.parseType()
  let name = self.expectToken(tkIdentifier)
  discard self.expectToken(tkEqual)
  return newDeclarationStatement(varType, name, self.parseExpr)

proc parseAssignment(self: var Parser): AssignmentStatement {.inline.} =
  let name = self.lexer.nextToken()
  discard self.expectToken(tkEqual)
  return newAssignmentStatement(name, self.parseExpr)

proc parseStmt(self: var Parser): Statement =
  let token = self.lexer.peekToken()

  if token.kind in {tkInt, tkUint}:
    return self.parseVariableDecl()
  elif token.kind == tkIdentifier:
    return self.parseAssignment()
  
  self.newError(errStatement, token, @{"@0": token.mean()})
  return newErrorStatement(token)

proc parse*(self: var Parser): BlockStatement =
  let startToken = self.lexer.peekToken()

  if startToken.kind == tkEOF:
    return newBlockStatement(startToken, startToken)

  var blockStatement = newBlockStatement(startToken, self.lexer.getEOFToken())

  while true:
    blockStatement.addStatement(self.parseStmt())
    if self.lexer.peekToken().kind == tkEOF:
      break
    discard self.expectToken(tkEOS)

  return blockStatement