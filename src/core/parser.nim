import lexer, astnodes, tokens, errors, types
import std/[tables, strutils, sets]

type Parser* = object
  file: string
  lexer: Lexer

const TOKEN_TYPE_KINDS = {
  tkInt64, tkInt32, tkInt16, tkInt8, 
  tkUint64, tkUint32, tkUint16, tkUint8, 
  tkBool, 
  tkChar
}
  
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
  of tkInt64: result = getInt64Type()
  of tkInt32: result = getInt32Type()
  of tkInt16: result = getInt16Type()
  of tkInt8: result = getInt8Type()
  of tkUint64: result = getUint64Type()
  of tkUint32: result = getUint32Type()
  of tkUint16: result = getUint16Type()
  of tkUint8: result = getUint8Type()
  of tkBool: result = getBoolType()
  of tkChar: result = getCharType()
  else: 
    self.newError(errProhibitedType, token, @{"@0": token.lexeme})
    return getUndefinedType()
  while self.lexer.peekToken().kind in {tkStar, tkLBracket}:
    let token = self.lexer.nextToken()

    if token.kind == tkStar:
      result = getPtrType(result)
    elif token.kind == tkLBracket:
      let token = self.lexer.nextToken()

      if token.kind == tkNumber:
        result = getStaticArrayType(result, parseInt(token.lexeme))
        discard self.expectToken(tkRBracket)
      elif token.kind == tkStar:
        result = getArrayType(result)
        discard self.expectToken(tkRBracket)
      elif token.kind == tkRBracket:
        result = getStaticArrayType(result, 0)
      else:
        self.newError(errSyntax, token)
        result = getUndefinedType()

proc parseType(self: var Parser): Type {.inline.} =
  let token = self.lexer.nextToken()
  self.parseType(token)

proc parseArguments(self: var Parser): OrderedTable[Token, Expression] =
  result = initOrderedTable[Token, Expression]()
  var pos = 0
  var usedNames: HashSet[string]

  if self.lexer.peekToken().kind == tkLParen:
    discard self.lexer.nextToken()
    
    while true:
      let rd = self.lexer.getRollbackData()
      let token = self.lexer.peekToken()

      if token.kind == tkIdentifier:
        discard self.lexer.nextToken()
        let next = self.lexer.nextToken()
        if next.kind == tkEqual:
          let value = self.parseExpr()
          if token.lexeme in usedNames:
            self.newError(errDuplicateArgument, token, @{"@0": token.lexeme})
          else:
            result[token] = value
            usedNames.incl(token.lexeme)
        else:
          self.lexer.rollback(rd)
          let expr = self.parseExpr()
          let posToken = token.newFrom(tkNumber, lexeme = $pos)
          result[posToken] = expr
          usedNames.incl(posToken.lexeme)
          pos.inc
      else:
        let expr = self.parseExpr()
        let posToken = expr.token.newFrom(tkNumber, lexeme = $pos)
        result[posToken] = expr
        usedNames.incl(posToken.lexeme)
        pos.inc
      
      if self.lexer.peekToken().kind in {tkRParen, tkEOF}:
        break
      
      discard self.expectToken(tkComma)
      
      if self.lexer.peekToken().kind == tkEOS:
        discard self.lexer.nextToken()
    
    discard self.expectToken(tkRParen)
  else:
    let expr = self.parseExpr()
    let posToken = expr.token.newFrom(tkNumber, lexeme = $pos)
    result[posToken] = expr

proc parseSpecialExpr(self: var Parser, name: Token): Expression =
  discard self.lexer.nextToken()

  var namedArgs: OrderedTable[Token, Expression] = initOrderedTable[Token, Expression]()

  if self.lexer.peekToken().kind == tkLParen:
    namedArgs = self.parseArguments()
  else:
    let expr = self.parseExpr()
    namedArgs[expr.token.newFrom(tkNumber, lexeme = "0")] = expr

  let specialKind = getSpecialExprKind(name)

  if specialKind == skExprError:
    return newErrorExpression(name)

  return newSpecialExpression(name, specialKind, namedArgs)

proc parsePrimary(self: var Parser): Expression =
  let token = self.lexer.nextToken()

  if token.kind == tkLParen:
    result = self.parseExpr()
    discard self.expectToken(tkRParen)
    return result

  elif token.kind == tkNumber:
    return newNumberExpression(token)

  elif token.kind == tkCharLiteral:
    return newCharExpression(token)

  elif token.kind == tkNul:
    return newNulExpression(token)

  elif token.kind in {tkTrue, tkFalse}:
    return newBoolExpression(token)

  elif token.kind == tkIdentifier:
    result = newIdentifierExpression(token)

    if self.lexer.peekToken().kind == tkColon:
      result = self.parseSpecialExpr(token)
    
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

  elif token.kind == tkStringLiteral:
    var arrayExpr = newArrayExpression(token)
    let str = token.lexeme
    
    for ch in str:
      let charToken = tkCharLiteral.newToken($ch, token.file, token.line, token.column, token.offset)
      arrayExpr.addExpr(newCharExpression(charToken))
    let charToken = tkCharLiteral.newToken("\0", token.file, token.line, token.column, token.offset)
    arrayExpr.addExpr(newCharExpression(charToken))
    
    return arrayExpr

  elif token.kind in TOKEN_TYPE_KINDS:
    return newTypeExpression(token, self.parseType(token))

  self.newError(errExpression, token, @{"@0": token.mean()})
  return newErrorExpression(token)

proc parsePostfix(self: var Parser): Expression =
  result = self.parsePrimary()

  while self.lexer.peekToken().kind in {tkArrow, tkLBracket}:
    let token = self.lexer.nextToken()

    if token.kind == tkArrow:
      let castType = self.parseType()
      result = newCastExpression(token, castType, result)

    elif token.kind == tkLBracket:
      let index = self.parseExpr()
      discard self.expectToken(tkRBracket)
      result = newIndexExpression(token, result, index)

proc parsePrefix(self: var Parser): Expression =
  let token = self.lexer.peekToken()
  if token.kind in {tkPlus, tkMinus, tkNot}:
    let token = self.lexer.nextToken()
    return newUnaryExpression(self.parsePrefix(), token)

  elif token.kind == tkDollar:
    discard self.lexer.nextToken()
    return newDerefExpression(token, self.parsePrefix())

  return self.parsePostfix()

proc parseMulDiv(self: var Parser): Expression =
  result = self.parsePrefix()

  while self.lexer.peekToken().kind in {tkStar, tkSlash, tkPercent}:
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
  var symbolType = self.parseType()
  let name = self.expectToken(tkIdentifier)
  if self.lexer.peekToken().kind == tkEqual:
    discard self.expectToken(tkEqual)
    return newDeclarationStatement(symbolType, name, self.parseExpr)
  return newDefaultStatement(symbolType, name)

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

proc parseBranchBlock(self: var Parser, startToken: Token): BlockStatement =
  let blockStmt = newBlockStatement(startToken)

  discard self.expectToken(tkEOS)

  while self.lexer.peekToken().kind notin {tkElif, tkElse, tkEnd, tkEOF}:
    blockStmt.addStatement(self.parseStmt())
    discard self.expectToken(tkEOS)

  if self.lexer.peekToken().kind == tkEOF:
    if startToken.kind != tkElse:
      newError(errExpectedSyntax, self.lexer.peekToken(), 
        @{"@0": "any of the keywords: elif, else or end", "@1": tkEOF.mean})
    else:
      newError(errExpectedSyntax, self.lexer.peekToken(), 
        @{"@0": tkEnd.mean, "@1": tkEOF.mean})

  blockStmt.endToken = self.lexer.peekToken()
  return blockStmt

proc parseBranching(self: var Parser): Statement =
  discard self.lexer.nextToken()
  let condition = self.parseExpr()
  let doToken = self.expectToken(tkDo)
  
  let ifBlock = self.parseBranchBlock(doToken)
  var branchingStatement = newBranchingStatement(condition, ifBlock)

  while true:
    let tok = self.lexer.peekToken()
    if tok.kind == tkEnd:
      break

    elif tok.kind == tkElse:
      discard self.lexer.nextToken()

      let elseBlock = self.parseBranchBlock(tok)
      branchingStatement.setElse(elseBlock)

      break

    elif tok.kind == tkElif:
      discard self.lexer.nextToken()

      let condition = self.parseExpr()
      let doToken = self.expectToken(tkDo)
      let elifBlock = self.parseBranchBlock(doToken)

      branchingStatement.addElif(condition, elifBlock)
      continue

    break

  discard self.expectToken(tkEnd)

  return branchingStatement

proc parseSpecialStmt(self: var Parser, left: Expression): Statement =
  let token = self.lexer.nextToken()
  
  if not (left of IdentifierExpression):
    self.newError(errStmtSpecial, token)
    return newErrorStatement(token)
  
  let name = IdentifierExpression(left).token
  var namedArgs: OrderedTable[Token, Expression] = initOrderedTable[Token, Expression]()
  
  if self.lexer.peekToken().kind == tkLParen:
    namedArgs = self.parseArguments()
  else:
    let expr = self.parseExpr()
    namedArgs[expr.token.newFrom(tkNumber, lexeme = "0")] = expr
  
  let specialKind = getSpecialStmtKind(name)
  
  if specialKind == skStmtError:
    return newErrorStatement(name)
  
  return newSpecialStatement(name, specialKind, namedArgs)

proc parseWhile(self: var Parser): Statement =
  let token = self.lexer.nextToken()

  let condition = self.parseExpr()

  let blockStmt = newBlockStatement(self.expectToken(tkDo))

  discard self.expectToken(tkEOS)

  while self.lexer.peekToken().kind notin {tkEnd, tkEOF}:
    blockStmt.addStatement(self.parseStmt())
    discard self.expectToken(tkEOS)

  discard self.expectToken(tkEnd)

  blockStmt.endToken = self.lexer.peekToken()

  return newWhileStatement(token, condition, blockStmt)

proc parseStmt(self: var Parser): Statement =
  let token = self.lexer.peekToken()

  if token.kind in TOKEN_TYPE_KINDS:
    return self.parseSymbolDecl()

  elif token.kind in {tkIdentifier, tkDollar}:
    let rd = self.lexer.getRollbackData()
    var left: Expression = newIdentifierExpression(self.lexer.nextToken())

    if self.lexer.peekToken().kind == tkColon:
      return self.parseSpecialStmt(left)
    else:
      self.lexer.rollback(rd)

    left = self.parsePrefix()
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

  elif token.kind == tkWhile:
    return self.parseWhile()

  elif token.kind == tkBreak:
    return newBreakStatement(self.lexer.nextToken())

  elif token.kind == tkContinue:
    return newContinueStatement(self.lexer.nextToken())
  
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