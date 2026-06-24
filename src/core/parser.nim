type Parser* = object
  text: string
  path: string
  
func newParser*(text, path: string): Parser =
  Parser(text: text, path: path)