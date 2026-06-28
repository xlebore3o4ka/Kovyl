import lexer, astnodes, tokens, errors
import ../utils/strtok

type Parser* = object
  file: string
  lexer: Lexer
  
proc newParser*(text, file: string): Parser =
  Parser(file: file, lexer: newLexer(text, file))

proc newError(self: Parser,
  kind: ErrorKind, file: string, token: Token, 
  args: seq[(string, string)] = @[]) =
  if not self.lexer.hasError:
    newError(kind, file, token, args)

proc expectToken*(self: var Parser, expected: TokenKind): Token =
  let token = self.lexer.nextToken()
  if token.kind != expected:
    self.newError(errExpectedSyntax, self.file, token, @{"@0": expected.mean(), "@1": token.mean()})
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

  self.newError(errExpression, self.file, token, @{"@0": token.mean()})
  return newErrorExpression(token)

proc parseUnary(self: var Parser): Expression =
  if self.lexer.peekToken().kind in {tkPlus, tkMinus}:
    let token = self.lexer.nextToken()
    var n = 1
    while self.lexer.peekToken().kind == token.kind:
      n.inc
      discard self.lexer.nextToken()
    if n mod 2 == 1:
      return newUnaryExpression(self.parseUnary(), token)

  return self.parsePrimary()

proc parseMulDiv(self: var Parser): Expression =
  var expression = self.parseUnary()

  while self.lexer.peekToken().kind in {tkStar, tkSlash}:
    let op = self.lexer.nextToken()
    let right = self.parseUnary()
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

proc parseVariableDecl(self: var Parser, token: Token): DeclarationStatement {.inline.} =
  let name = self.expectToken(tkIdentifier)
  discard self.expectToken(tkEqual)
  return newDeclarationStatement(token, name, self.parseExpr)

proc parseAssignment(self: var Parser, name: Token): AssignmentStatement {.inline.} =
  discard self.expectToken(tkEqual)
  return newAssignmentStatement(name, self.parseExpr)

proc parseStmt(self: var Parser): Statement =
  let token = self.lexer.nextToken()

  if token.kind in {tkInt}:
    return self.parseVariableDecl(token)
  elif token.kind == tkIdentifier:
    return self.parseAssignment(token)
  
  self.newError(errStatement, self.file, token, @{"@0": token.mean()})
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