type
  TokenKind* = enum
    tkIntLiteral
    tkStringLiteral
    tkCharLiteral
    tkIdentifier

    tkPlus, tkMinus, tkStar, tkSlash
    tkEQ, tkNEQ, tkGT, tkLT, tkGTE, tkLTE
    tkEqual, tkNot
    tkColon, tkComma, tkArrow
    tkHash, tkPragma

    tkLParen, tkRParen
    tkLBracket, tkRBracket
    tkLBrace, tkRBrace

    tkAnd
    tkOr

    tkTrue
    tkFalse

    tkInt
    tkUint
    tkBool
    tkChar

    tkDo, tkEnd
    tkIf, tkElif, tkElse

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
  of tkIntLiteral: return "int literal"
  of tkStringLiteral: return "string literal"
  of tkCharLiteral: return "character literal"
  of tkIdentifier: return "identifier"
  of tkPlus: return "plus operator '+'"
  of tkMinus: return "minus operator '-'"
  of tkStar: return "star operator '*'"
  of tkSlash: return "slash operator '/'"
  of tkEQ: return "equals operator '=='"
  of tkNEQ: return "not equals operator '!='"
  of tkGT: return "greater operator '>'"
  of tkLT: return "less operator '<'"
  of tkGTE: return "greater or equal operator '>='"
  of tkLTE: return "less or equal operator '<='"
  of tkEqual: return "equal operator '='"
  of tkNot: return "not operator '!'"
  of tkColon: return "colon operator ':'"
  of tkComma: return "comma operator ','"
  of tkArrow: return "arrow operator '->'"
  of tkHash: return "hash operator '#'"
  of tkPragma: return "pragma operator '#!'"
  of tkLParen: return "left parenthesis '('"
  of tkRParen: return "right parenthesis ')'"
  of tkLBracket: return "left bracket '['"
  of tkRBracket: return "right bracket ']'"
  of tkLBrace: return "left brace '{'"
  of tkRBrace: return "right brace '}'"
  of tkAnd: return "and operator 'and'"
  of tkOr: return "or operator 'or'"
  of tkInt: return "int type"
  of tkUint: return "uint type"
  of tkBool: return "bool type"
  of tkChar: return "char type"
  of tkTrue: return "true literal"
  of tkFalse: return "false literal"
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"
  of tkInvalid: return "invalid token"
  of tkDo: return "keyword do"
  of tkEnd: return "keyword end"
  of tkIf: return "keyword if"
  of tkElif: return "keyword elif"
  of tkElse: return "keyword else"

proc mean*(token: Token): string =
  mean(token.kind)