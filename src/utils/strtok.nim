import ../core/tokens

proc mean*(token: Token): string =
  case token.kind:
  of tkIntLiteral: return "number " & token.lexeme
  of tkIdentifier: return "identifier " & token.lexeme
  of tkPlus: return "plus operator"
  of tkMinus: return "minus operator"
  of tkStar: return "star operator"
  of tkSlash: return "slash operator"
  of tkEqual: return "equal operator"
  of tkInt: return "int keyword"
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"
  of tkInvalid: return "invalid token"

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
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"
  of tkInvalid: return "invalid token"