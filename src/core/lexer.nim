import std/[unicode, tables]
import tokens

type Lexer* = object
  text: string
  len: Natural
  line: Positive = 1
  column: Positive = 1
  pos: Natural = 0

func newLexer*(text: string): Lexer =
  Lexer(text: text, len: text.len.Natural)

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

func nextToken*(self: var Lexer): Token =
  while self.peek in ['\t'.Rune, ' '.Rune, '\r'.Rune]: self.advance
  let c = self.peek
  if c == Rune(0): return tkEOF.newToken("\0", self.line + 1, 1, self.pos)
  elif c == '\n'.Rune: 
    result = tkEOS.newToken("\\n", self.line, self.column, self.pos)
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
    result = tkNumber.newToken(num, self.line, column, start)
  elif c in operatorTokens:
    result = operatorTokens[c].newToken($c, self.line, self.column, self.pos)
    self.advance()
  else:
    return tkEOF.newToken("\0", self.line, 1, self.pos)