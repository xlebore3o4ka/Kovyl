import std/[unicode, tables]
import tokens, errors

type Lexer* = object
  text: string
  file: string
  len: Natural
  line: Positive = 1
  column: Positive = 1
  pos: Natural = 0

  peekedToken*: Token
  hasPeeked*: bool = false

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
  '/'.Rune: tkSlash
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
  else:
    newError(errSyntax, self.file, self.line, self.column, self.pos, 1)
    self.advance()
    return self.nextToken()

proc peekToken*(self: var Lexer): Token =
  if not self.hasPeeked:
    self.peekedToken = self.nextToken()
    self.hasPeeked = true
  return self.peekedToken