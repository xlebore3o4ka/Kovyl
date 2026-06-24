type
  TokenType* = enum
    tkNumber,
    tkPlus, tkMinus, tkStar, tkSlash,
    tkEOS,
    tkEOF

  Token* = object
    kind*: TokenType
    value*: string
    line*: Positive
    column*: Positive
    offset*: Natural

func newToken*(kind: TokenType, value: string, line: Positive, column: Positive, offset: Natural): Token =
  Token(kind: kind, value: value, line: line, column: column, offset: offset)