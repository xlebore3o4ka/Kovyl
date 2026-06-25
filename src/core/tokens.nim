type
  TokenType* = enum
    tkIntLiteral,
    tkPlus, tkMinus, tkStar, tkSlash,
    tkEOS,
    tkEOF

  Token* = object
    kind*: TokenType
    lexeme*: string
    file*: string
    line*: Positive
    column*: Positive
    offset*: Natural

func newToken*(
              kind: TokenType, lexeme: string, file: string, 
              line: Positive, column: Positive, offset: Natural): Token =
  Token(kind: kind, lexeme: lexeme, file: file, line: line, column: column, offset: offset)