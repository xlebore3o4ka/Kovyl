type
  TokenKind* = enum
    tkIntLiteral,
    tkIdentifier,

    tkPlus, tkMinus, tkStar, tkSlash,
    tkEqual,

    tkLParen, tkRParen,

    tkInt,

    tkEOS,
    tkEOF,
    tkInvalid

  Token* = object
    kind*: TokenKind
    lexeme*: string
    file*: string
    line*: Positive = 1
    column*: Positive = 1
    offset*: Natural

func newToken*(
              kind: TokenKind, lexeme: string, file: string, 
              line: Positive, column: Positive, offset: Natural): Token =
  Token(kind: kind, lexeme: lexeme, file: file, line: line, column: column, offset: offset)