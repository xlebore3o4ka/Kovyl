import ../core/tokens

proc mean*(kind: TokenKind): string =
  case kind:
  of tkIntLiteral: return "number"
  of tkIdentifier: return "identifier"
  of tkPlus: return "plus operator"
  of tkMinus: return "minus operator"
  of tkStar: return "star operator"
  of tkSlash: return "slash operator"
  of tkEqual: return "equal operator"
  of tkInt: return "int keyword"
  of tkLParen: return "left parenthesis"
  of tkRParen: return "right parenthesis"
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"
  of tkInvalid: return "invalid token"

proc mean*(token: Token): string =
  mean(token.kind)