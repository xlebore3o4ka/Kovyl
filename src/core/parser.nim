import lexer, tokens, errors, ast

type Parser* = object
  file: string
  lexer: Lexer

proc newParser*(text, file: string): Parser =
  Parser(file: file, lexer: newLexer(text, file))

proc newError(self: var Parser,
  kind: ErrorKind, token: Token, 
  args: varargs[string, `$`]) =
  if not self.lexer.wasError:
    newError(kind, token, args)

  let currentLine = token.line
  while self.lexer.peekToken().line == currentLine and self.lexer.peekToken().kind != tkEOF:
    discard self.lexer.nextToken()

proc nextToken(self: var Parser): Token {.inline.} = self.lexer.nextToken()
proc peekToken(self: var Parser): Token {.inline.} = self.lexer.peekToken()

proc expectToken(self: var Parser, expected: TokenKind): Token =
  let token = self.nextToken()
  if token.kind != expected:
    self.newError(errExpectedSyntax, token, expected.mean, token.mean)
    return token.copy(kind = tkInvalid)
  return token

proc parseExpression*(self: var Parser): Expression

proc parsePrimary*(self: var Parser): Expression =
  let token = self.nextToken()

  if token.kind == tkLParen:
    result = self.parseExpression()
    discard self.expectToken(tkRParen)
    return result

  elif token.kind == tkNumber:
    return newNumberExpression(token)

  elif token.kind in {tkTrue, tkFalse}:
    return newBoolExpression(token)

  self.newError(errExpression, token, token.mean)
  return newInvalidExpression(token)

proc parsePrefix(self: var Parser): Expression =
  let token = self.peekToken()
  if token.kind in {tkPlus, tkMinus, tkBang}:
    let token = self.nextToken()
    return newUnaryExpression(token, self.parsePrefix())

  return self.parsePrimary()

proc parseMulDiv(self: var Parser): Expression =
  result = self.parsePrefix()

  while self.peekToken().kind in {tkStar, tkSlash, tkPercent}:
    let op = self.nextToken()
    let right = self.parsePrefix()
    result = newBinaryExpression(op, result, right)

proc parseAddSub(self: var Parser): Expression =
  result = self.parseMulDiv()

  while self.peekToken().kind in {tkPlus, tkMinus}:
    let op = self.nextToken()
    let right = self.parseMulDiv()
    result = newBinaryExpression(op, result, right)

proc parseComparison(self: var Parser): Expression =
  result = self.parseAddSub()

  while self.lexer.peekToken().kind in {tkGT, tkLT, tkGTE, tkLTE, tkEqualsEquals, tkBangEquals}:
    let op = self.lexer.nextToken()
    let right = self.parseAddSub()
    result = newBinaryExpression(op, result, right)

proc parseAnd(self: var Parser): Expression =
  result = self.parseComparison()

  while self.lexer.peekToken().kind == tkAnd:
    let op = self.lexer.nextToken()
    let right = self.parseComparison()
    result = newBinaryExpression(op, result, right)

proc parseOr(self: var Parser): Expression =
  result = self.parseAnd()

  while self.lexer.peekToken().kind == tkOr:
    let op = self.lexer.nextToken()
    let right = self.parseAnd()
    result = newBinaryExpression(op, result, right)

proc parseExpression*(self: var Parser): Expression =
  return self.parseOr()

proc parse*(self: var Parser): Expression =
  return self.parseExpression()