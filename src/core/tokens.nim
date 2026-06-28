type
  TokenKind* = enum
    tkIntLiteral
    tkIdentifier

    tkPlus, tkMinus, tkStar, tkSlash
    tkEqual
    tkColon

    tkLParen, tkRParen

    tkTrue
    tkFalse

    tkInt
    tkUint
    tkBool

    tkEOS
    tkEOF
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

proc mean*(kind: TokenKind): string =
  case kind:
  of tkIntLiteral: return "number literal"
  of tkIdentifier: return "identifier"
  of tkPlus: return "plus operator '+'"
  of tkMinus: return "minus operator '-'"
  of tkStar: return "star operator '*'"
  of tkSlash: return "slash operator '/'"
  of tkEqual: return "equal operator '='"
  of tkColon: return "colon operator ':'"
  of tkLParen: return "left parenthesis '('"
  of tkRParen: return "right parenthesis ')'"
  of tkInt: return "int type"
  of tkUint: return "uint type"
  of tkBool: return "bool type"
  of tkTrue: return "true literal"
  of tkFalse: return "false literal"
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"
  of tkInvalid: return "invalid token"

proc mean*(token: Token): string =
  mean(token.kind)