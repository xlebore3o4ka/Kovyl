import lexer, astnodes, tokens, errors
import ../utils/strtok
import std/tables

type Parser* = object
  file: string
  lexer: Lexer
  
proc newParser*(text, file: string): Parser =
  Parser(file: file, lexer: newLexer(text, file))

proc parsePrimary(self: var Parser): Expression =
  let token = self.lexer.nextToken()
  if token.kind == tkIntLiteral:
    return newIntLitExpression(token)
  newError(errExpression, self.file, token, {"@0": token.mean()}.toTable)
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

proc parse*(self: var Parser): Expression =
  self.parseExpr()