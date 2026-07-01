import lexer, astnodes, tokens, errors, types
import std/tables

type Parser* = object
  file: string
  lexer: Lexer
  
proc newParser*(text, file: string): Parser =
  Parser(file: file, lexer: newLexer(text, file))

proc newError(self: var Parser,
  kind: ErrorKind, token: Token, 
  args: seq[(string, string)] = @[]) =
  if not self.lexer.hasError:
    newError(kind, token, args)
    while self.lexer.peekToken().kind notin {tkEOS, tkEOF}:
      discard self.lexer.nextToken()

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
    return newIntExpression(token)

  elif token.kind in {tkTrue, tkFalse}:
    return newBoolExpression(token)

  elif token.kind == tkIdentifier:
    return newIdentifierExpression(token)

  self.newError(errExpression, token, @{"@0": token.mean()})
  return newErrorExpression(token)

proc parseUnary(self: var Parser): Expression =
  if self.lexer.peekToken().kind in {tkPlus, tkMinus, tkNot}:
    let token = self.lexer.nextToken()
    return newUnaryExpression(self.parseUnary(), token)

  return self.parsePrimary()

proc parseType(self: var Parser): ptr Type =
  let token = self.lexer.nextToken()
  case token.kind:
  of tkInt: return getIntType()
  of tkUint: return getUintType()
  of tkBool: return getBoolType()
  of tkString: return getStringType()
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

proc parseComparison(self: var Parser): Expression =
  var expression = self.parseAddSub()

  while self.lexer.peekToken().kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}:
    let op = self.lexer.nextToken()
    let right = self.parseAddSub()
    expression = newBinaryExpression(expression, op, right)

  return expression

proc parseAnd(self: var Parser): Expression =
  var expression = self.parseComparison()

  while self.lexer.peekToken().kind == tkAnd:
    let op = self.lexer.nextToken()
    let right = self.parseComparison()
    expression = newBinaryExpression(expression, op, right)

  return expression

proc parseOr(self: var Parser): Expression =
  var expression = self.parseAnd()

  while self.lexer.peekToken().kind == tkOr:
    let op = self.lexer.nextToken()
    let right = self.parseAnd()
    expression = newBinaryExpression(expression, op, right)

  return expression

proc parseExpr(self: var Parser): Expression =
  return self.parseOr()

proc parseSymbolDecl(self: var Parser): Statement {.inline.} =
  var varType = self.parseType()
  let name = self.expectToken(tkIdentifier)
  discard self.expectToken(tkEqual)
  return newDeclarationStatement(varType, name, self.parseExpr)

proc parseAssignment(self: var Parser): Statement {.inline.} =
  let name = self.lexer.nextToken()
  if self.lexer.peekToken().kind == tkIdentifier:
    self.newError(errUnknownType, name)
    return newErrorStatement(self.lexer.nextToken())
  discard self.expectToken(tkEqual)
  return newAssignmentStatement(name, self.parseExpr)

type
  pragmaProc = (proc (self: var Parser): Statement)

proc pragmaOut(self: var Parser): Statement =
  return newOutStatement(self.parseExpr())

const pragmaMap: Table[string, pragmaProc] = {
  "out": pragmaProc(pragmaOut)
}.toTable

proc parsePragma(self: var Parser): Statement =
  let token = self.lexer.nextToken()
  let name = self.expectToken(tkIdentifier)
  discard self.expectToken(tkLParen)

  if name.lexeme notin pragmaMap:
    self.newError(errUnknownPragma, name)
    return newErrorStatement(token)

  result = pragmaMap[name.lexeme](self)

  discard self.expectToken(tkRParen)

proc parseStmt(self: var Parser): Statement

proc parseBranching(self: var Parser): Statement =
  discard self.lexer.nextToken()
  let condition = self.parseExpr()

  let ifBlock = newBlockStatement(self.expectToken(tkDo))
  discard self.expectToken(tkEOS)

  while self.lexer.peekToken().kind notin {tkElif, tkElse, tkEnd}:
    ifBlock.addStatement(self.parseStmt())
    discard self.expectToken(tkEOS)

  ifBlock.endToken = self.lexer.peekToken()

  var branchingStatement = newBranchingStatement(condition, ifBlock)

  while true:
    let tok = self.lexer.peekToken()
    if tok.kind == tkEnd: break
    elif tok.kind == tkElse:
      discard self.lexer.nextToken()
      let elseBlock = newBlockStatement(tok)
      discard self.expectToken(tkEOS)

      while self.lexer.peekToken().kind != tkEnd:
        elseBlock.addStatement(self.parseStmt())
        discard self.expectToken(tkEOS)

      elseBlock.endToken = self.lexer.peekToken()

      branchingStatement.setElse(elseBlock)
      break
    elif tok.kind == tkElif:
      discard self.lexer.nextToken()
      let condition = self.parseExpr()

      let elifBlock = newBlockStatement(self.expectToken(tkDo))
      discard self.expectToken(tkEOS)

      while self.lexer.peekToken().kind notin {tkElif, tkElse, tkEnd}:
        elifBlock.addStatement(self.parseStmt())
        discard self.expectToken(tkEOS)

      elifBlock.endToken = self.lexer.peekToken()

      branchingStatement.addElif(condition, elifBlock)
      continue
    break

  discard self.expectToken(tkEnd)

  return branchingStatement

proc parseStmt(self: var Parser): Statement =
  let token = self.lexer.peekToken()

  if token.kind in {tkInt, tkUint, tkBool, tkString}:
    return self.parseSymbolDecl()
  elif token.kind == tkIdentifier:
    return self.parseAssignment()
  elif token.kind == tkPragma:
    return self.parsePragma()
  elif token.kind == tkIf:
    return self.parseBranching()
  
  self.newError(errStatement, token, @{"@0": token.mean()})
  return newErrorStatement(token)

proc parse*(self: var Parser): BlockStatement =
  let startToken = self.lexer.peekToken()

  if startToken.kind == tkEOF:
    return newBlockStatement(startToken, startToken)

  var blockStatement = newBlockStatement(startToken, self.lexer.getEOFToken())

  if self.lexer.peekToken().kind == tkEOS:
    discard self.lexer.nextToken()

  while true:
    blockStatement.addStatement(self.parseStmt())
    if self.lexer.peekToken().kind == tkEOF:
      break
    discard self.expectToken(tkEOS)
    if self.lexer.peekToken().kind == tkEOF:
      break

  return blockStatement