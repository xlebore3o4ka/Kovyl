import std/[unicode, tables]
import tokens, errors
import ../utils/strtok

type Lexer* = object
  text: string
  file: string
  len: Natural
  line: Positive = 1
  column: Positive = 1
  pos: Natural = 0

  peekedToken*: Token
  hasPeeked*: bool = false

proc getEOFToken*(self: Lexer): Token =
  let lastPos = if self.len > 0: self.len - 1 else: 0
  let lastRune = self.text.runeAt(lastPos)
  
  var line = 1
  var column = 1
  var pos = 0
  
  while pos < lastPos:
    let r = self.text.runeAt(pos)
    if r == '\n'.Rune:
      line.inc
      column = 1
    else:
      column.inc
    pos += r.size
  
  if lastRune == '\n'.Rune:
    return tkEOF.newToken("\0", self.file, line + 1, 1, self.len)
  else:
    return tkEOF.newToken("\0", self.file, line, column + 1, self.len)

func newLexer*(text: string, file: string): Lexer =
  Lexer(text: text, file: file, len: text.len.Natural, peekedToken: tkEOF.newToken("\0", file, 1, 1, 0))

func peek(self: Lexer): Rune {.inline.} =
  if self.pos >= self.len: return Rune(0)
  self.text.runeAt(self.pos)

func advance(self: var Lexer) {.inline.} = 
  self.pos += self.text.runeAt(self.pos).size
  self.column.inc

func isDigit(c: Rune): bool {.inline.} = int(c) in 48..57

const operatorTokens = {
  '+'.Rune: tkPlus,
  '-'.Rune: tkMinus,
  '*'.Rune: tkStar,
  '/'.Rune: tkSlash,
  '='.Rune: tkEqual
}.toTable

const keywordsTokens = {
  "int": tkInt
}.toTable

proc nextToken*(self: var Lexer): Token =
  if self.hasPeeked:
    self.hasPeeked = false
    return self.peekedToken
  while self.peek in ['\t'.Rune, ' '.Rune, '\r'.Rune]: self.advance
  let c = self.peek
  if c == Rune(0): return tkEOF.newToken("\0", self.file, self.line, self.column, self.pos)
  elif c == '\n'.Rune: 
    result = tkEOS.newToken("\\n", self.file, self.line, self.column, self.pos)
    self.line.inc
    self.advance
    self.column = 1
  elif c.isDigit:
    let column = self.column
    var start = self.pos
    var num = ""
    while self.peek.isDigit:
      num &= $self.peek
      self.advance()
    result = tkIntLiteral.newToken(num, self.file, self.line, column, start)
  elif c in operatorTokens:
    result = operatorTokens[c].newToken($c, self.file, self.line, self.column, self.pos)
    self.advance()
  elif c.isAlpha or c == '_'.Rune:
    let column = self.column
    var start = self.pos
    var ident = ""

    while (let p = self.peek; p.isDigit or p.isAlpha or p == '_'.Rune):
      ident &= $p
      self.advance()

    if ident in keywordsTokens:
      result = keywordsTokens[ident].newToken(ident, self.file, self.line, column, start)
    else:
      result = tkIdentifier.newToken(ident, self.file, self.line, column, start)
  else:
    newError(errSyntax, self.file, self.line, self.column, self.pos, 1)
    self.advance()
    return self.nextToken()

proc peekToken*(self: var Lexer): Token =
  if not self.hasPeeked:
    self.peekedToken = self.nextToken()
    self.hasPeeked = true
  return self.peekedToken

proc expectToken*(self: var Lexer, expected: TokenKind): Token =
  let token = self.nextToken()
  if token.kind != expected:
    newError(errExpectedSyntax, self.file, token, {"@0": expected.mean(), "@1": token.mean()}.toTable)
    return tkInvalid.newToken(token.lexeme, self.file, token.line, token.column, token.offset)
  return token