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

proc parseType(self: var Parser, token: Token): Type =
  case token.kind:
  of tkInt: result = getIntType()
  of tkUint: result = getUintType()
  of tkBool: result = getBoolType()
  of tkChar: result = getCharType()
  of tkLBracket:
    let baseType = self.parseType(self.lexer.nextToken())
    discard self.expectToken(tkRBracket)
    result = getArrayType(baseType)
  else: 
    self.newError(errSyntax, token)
    return getUndefinedType()
  while self.lexer.peekToken().kind == tkStar:
    discard self.lexer.nextToken()
    result = getPtrType(result)

proc parseType(self: var Parser): Type {.inline.} =
  let token = self.lexer.nextToken()
  self.parseType(token)

proc parsePrimary(self: var Parser): Expression =
  let token = self.lexer.nextToken()

  if token.kind == tkLParen:
    result = self.parseExpr()
    discard self.expectToken(tkRParen)
    return result

  elif token.kind == tkIntLiteral:
    return newIntExpression(token)

  elif token.kind == tkCharLiteral:
    return newCharExpression(token)

  elif token.kind in {tkTrue, tkFalse}:
    return newBoolExpression(token)

  elif token.kind == tkIdentifier:
    result = newIdentifierExpression(token)

    if self.lexer.peekToken().kind == tkColon:
      discard self.lexer.nextToken()
      let name = token
      var expectParen = false

      if self.lexer.peekToken().kind == tkLParen:
        expectParen = true
        discard self.lexer.nextToken()

      if name.lexeme == "new":
        result = newNewExpression(name, self.parseExpr())
      else:
        self.newError(errSpecial, name)
        result = newErrorExpression(name)

      if expectParen:
        if (let t = self.expectToken(tkRParen); t.kind == tkInvalid):
          result = newErrorExpression(t)
    
    return result

  elif token.kind == tkLBrace:
    var arrayExpr = newArrayExpression(token)

    while self.lexer.peekToken().kind != tkRBrace:
      arrayExpr.addExpr(self.parseExpr())

      if self.lexer.peekToken().kind == tkRBrace: 
        break

      discard self.expectToken(tkComma)

      if self.lexer.peekToken().kind == tkEOS:
        discard self.lexer.nextToken()

    discard self.expectToken(tkRBrace)
    
    return arrayExpr

  self.newError(errExpression, token, @{"@0": token.mean()})
  return newErrorExpression(token)

proc parsePrefix(self: var Parser): Expression =
  if self.lexer.peekToken().kind in {tkPlus, tkMinus, tkNot}:
    let token = self.lexer.nextToken()
    return newUnaryExpression(self.parsePrefix(), token)

  return self.parsePrimary()

proc parsePostfix(self: var Parser): Expression =
  result = self.parsePrefix()

  while self.lexer.peekToken().kind in {tkArrow, tkLBracket}:
    let token = self.lexer.nextToken()

    if token.kind == tkArrow:
      let castType = self.parseType()
      result = newCastExpression(token, castType, result)

    elif token.kind == tkLBracket:
      if self.lexer.peekToken().kind == tkRBracket:
        discard self.lexer.nextToken()
        result = newDerefExpression(token, result)

      else:
        let index = self.parseExpr()
        discard self.expectToken(tkRBracket)
        result = newIndexExpression(token, result, index)

proc parseMulDiv(self: var Parser): Expression =
  result = self.parsePostfix()

  while self.lexer.peekToken().kind in {tkStar, tkSlash}:
    let op = self.lexer.nextToken()
    let right = self.parsePostfix()
    result = newBinaryExpression(result, op, right)

proc parseAddSub(self: var Parser): Expression =
  result = self.parseMulDiv()

  while self.lexer.peekToken().kind in {tkPlus, tkMinus}:
    let op = self.lexer.nextToken()
    let right = self.parseMulDiv()
    result = newBinaryExpression(result, op, right)

proc parseComparison(self: var Parser): Expression =
  result = self.parseAddSub()

  while self.lexer.peekToken().kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}:
    let op = self.lexer.nextToken()
    let right = self.parseAddSub()
    result = newBinaryExpression(result, op, right)

proc parseAnd(self: var Parser): Expression =
  result = self.parseComparison()

  while self.lexer.peekToken().kind == tkAnd:
    let op = self.lexer.nextToken()
    let right = self.parseComparison()
    result = newBinaryExpression(result, op, right)

proc parseOr(self: var Parser): Expression =
  result = self.parseAnd()

  while self.lexer.peekToken().kind == tkOr:
    let op = self.lexer.nextToken()
    let right = self.parseAnd()
    result = newBinaryExpression(result, op, right)

proc parseExpr(self: var Parser): Expression =
  return self.parseOr()

proc parseSymbolDecl(self: var Parser): Statement {.inline.} =
  var varType = self.parseType()
  let name = self.expectToken(tkIdentifier)
  discard self.expectToken(tkEqual)
  return newDeclarationStatement(varType, name, self.parseExpr)

proc parseAssignment(self: var Parser, left: Expression): Statement {.inline.} =
  discard self.expectToken(tkEqual)
  
  if left of DerefExpression:
    return newAssignmentStatement(DerefExpression(left), self.parseExpr())
  elif left of IdentifierExpression:
    return newAssignmentStatement(IdentifierExpression(left), self.parseExpr())
  elif left of IndexExpression:
    return newAssignmentStatement(IndexExpression(left), self.parseExpr())
  else:
    self.newError(errCannotAssign, left.token)
    return newErrorStatement(left.token)

type
  pragmaProc = (proc (self: var Parser): Statement)

const pragmaMap: Table[string, pragmaProc] = initTable[string, pragmaProc]()

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

proc parseSpecialStmt(self: var Parser, left: Expression): Statement =
  let token = self.lexer.nextToken()
  
  if not (left of IdentifierExpression):
    self.newError(errSpecial, token)
    return newErrorStatement(token)

  var expectParen = false
  defer:
    if expectParen:
      discard self.lexer.nextToken()

  if self.lexer.peekToken().kind == tkLParen:
    expectParen = true
    discard self.lexer.nextToken()
  
  let name = IdentifierExpression(left).token
  if name.lexeme == "out":
    var res: OutStatement = newOutStatement()

    if expectParen:
      while true:
        res.addExpr(self.parseExpr())

        if self.lexer.peekToken().kind == tkRParen:
          break

        discard self.expectToken(tkComma)

        if self.lexer.peekToken().kind == tkEOS:
          discard self.lexer.nextToken()
    else:
      res.addExpr(self.parseExpr())

    return res

  if name.lexeme == "free":
    return newFreeStatement(self.parseExpr())

  else:
    self.newError(errSpecial, token)
    return newErrorStatement(token)

proc parseStmt(self: var Parser): Statement =
  let token = self.lexer.peekToken()

  if token.kind in {tkInt, tkUint, tkBool, tkChar, tkLBracket}:
    return self.parseSymbolDecl()

  elif token.kind == tkIdentifier:
    let rd = self.lexer.getRollbackData()
    var left: Expression = newIdentifierExpression(self.lexer.nextToken())

    if self.lexer.peekToken().kind == tkColon:
      return self.parseSpecialStmt(left)
    else:
      self.lexer.rollback(rd)

    left = self.parsePostfix()
    let token = self.lexer.peekToken()

    if token.kind == tkEqual:
      return self.parseAssignment(left)

    else:
      self.newError(errStatement, token, @{"@0": token.mean()})
      return newErrorStatement(token)

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