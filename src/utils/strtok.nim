import ../core/tokens

proc mean*(token: Token): string =
  case token.kind:
  of tkIntLiteral: return "number " & token.lexeme
  of tkPlus: return "plus"
  of tkMinus: return "minus"
  of tkStar: return "star"
  of tkSlash: return "slash"
  of tkEOS: return "end of statement"
  of tkEOF: return "end of file"