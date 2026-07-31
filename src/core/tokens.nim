import std/tables

type
  TokenKind* = enum
    tkNumber
    tkIdent

    tkPlus, tkMinus, tkStar, tkSlash, tkPercent
    tkGT, tkLT, tkGTE, tkLTE, tkEqualsEquals, tkBangEquals
    tkEquals, tkBang

    tkLParen, tkRParen

    tkInt64, tkBool
    tkAnd, tkOr, tkTrue, tkFalse

    tkEOF
    tkInvalid

  Token* = object
    kind*: TokenKind
    lexeme*: string
    file*: string
    line*: Positive = 1
    col*: Positive = 1
    len*: Natural

func newToken*(
    kind: TokenKind, lexeme: string, file: string, 
    line: Positive, col: Positive, len: Natural): Token =
  Token(kind: kind, lexeme: lexeme, file: file, line: line, col: col, len: len)

const operatorTable* = {

  "+": tkPlus,
  "-": tkMinus,
  "*": tkStar,
  "/": tkSlash,
  "%": tkPercent,

  ">": tkGT,
  "<": tkLT,
  ">=": tkGTE,
  "<=": tkLTE,
  "==": tkEqualsEquals,
  "!=": tkBangEquals,

  "!": tkBang,
  "=": tkEquals,

  "(": tkLParen,
  ")": tkRParen

}.toTable

const parenthesisTable* = {

  tkLParen: tkRParen

}.toTable
const rightParenthesis* = block:
  var res: seq[TokenKind] = @[]
  for v in values(parenthesisTable):
    res.add(v)
  res

const keywordTable* = {

  "int": tkInt64,
  "int64": tkInt64,
  "bool": tkBool,
  "true": tkTrue,
  "false": tkFalse,
  "and": tkAnd,
  "or": tkOr
  
}.toTable

func copy*(token: Token, kind: TokenKind = token.kind, lexeme: string = token.lexeme,
           file: string = token.file, line: Positive = token.line,
           col: Positive = token.col, len: Natural = token.len): Token =
  Token(kind: kind, lexeme: lexeme, file: file, line: line, col: col, len: len)

proc mean*(kind: TokenKind): string =
  case kind:
  of tkNumber:        return "number"
  of tkIdent:         return "identifier"
  
  of tkPlus:          return "plus operator '+'"
  of tkMinus:         return "minus operator '-'"
  of tkStar:          return "star operator '*'"
  of tkSlash:         return "slash operator '/'"
  of tkPercent:       return "percent operator '%'"
  
  of tkGT:            return "greater than operator '>'"
  of tkLT:            return "less than operator '<'"
  of tkGTE:           return "greater or equal operator '>='"
  of tkLTE:           return "less or equal operator '<='"
  of tkEqualsEquals:  return "equal operator '=='"
  of tkBangEquals:    return "not equal operator '!='"
  of tkBang:          return "not operator '!'"
  of tkEquals:        return "assign operator '='"
  
  of tkLParen:        return "left parenthesis '('"
  of tkRParen:        return "right parenthesis ')'"

  of tkInt64:         return "type 'int64'"
  of tkBool:          return "type 'bool'"
  
  of tkAnd:           return "keyword 'and'"
  of tkOr:            return "keyword 'or'"
  of tkTrue:          return "keyword 'true'"
  of tkFalse:         return "keyword 'false'"
  
  of tkInvalid:       return "invalid token"
  of tkEOF:           return "end of file"

proc mean*(token: Token): string =
  mean(token.kind)