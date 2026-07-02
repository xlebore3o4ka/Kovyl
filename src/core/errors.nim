import tokens

type
  ErrorKind* = enum
    errSyntax, errExpression, errStatement, errExpectedSyntax
    errMismatchedBracket, errUnexpectedBracket, errUnclosedBracket, errUnclosedString
    errBinaryTypeMismatch, errUnaryTypeMismatch, errTypeMismatch, errUnknownType, errCannotCast
    errRedeclaration, errUndeclaredSymbol, errSpecial
    errUnknownPragma
    errSpecialArgumentsNumber

  CompileError* = ref object
    kind*: ErrorKind
    file*: string
    line*: Positive
    col*: Positive
    pos*: Natural
    len*: Positive
    args*: seq[(string, string)]
    message*: string

var errors*: seq[CompileError] = @[]

proc message(kind: ErrorKind): string =
  case kind
    of errSyntax: "Invalid syntax"
    of errExpression: "Expected expression, got @0"
    of errStatement: "Expected statement, got @0"
    of errExpectedSyntax: "Expected @0, got @1"
    of errMismatchedBracket: "Mismatched bracket"
    of errUnexpectedBracket: "Unexpected closing bracket"
    of errUnclosedBracket: "Unclosed bracket"
    of errUnclosedString: "Unclosed string literal"
    of errBinaryTypeMismatch: "Type mismatch for binary operator '@0' (@1 @0 @2)"
    of errUnaryTypeMismatch: "Type mismatch for unary operator '@0' (@1)"
    of errTypeMismatch: "Type mismatch for @0 (expected @0, got @1)"
    of errUnknownType: "Unknown type"
    of errCannotCast: "Cannot cast from @0 to @1"
    of errRedeclaration: "Redeclaration of symbol '@0', originally declared at @1(@2:@3)"
    of errUndeclaredSymbol: "Undeclared symbol '@0'"
    of errSpecial: "Unknown special"
    of errUnknownPragma: "Unknown pragma"
    of errSpecialArgumentsNumber: "Invalid number of arguments for the '@0' special (expected @1)"

proc newError*(
              kind: ErrorKind, token: Token, 
              args: seq[(string, string)] = @[]) =
  let msg = kind.message()
  
  errors.add(CompileError(
    kind: kind, file: token.file, line: token.line, col: token.column,
    pos: token.offset, len: token.lexeme.len, args: args, message: msg
  ))

proc newError*(
              kind: ErrorKind, file: string, line: Positive, col: Positive, 
              pos: Natural, len: Positive, 
              args: seq[(string, string)] = @[]) =
  let msg = kind.message()
  
  errors.add(CompileError(
    kind: kind, file: file, line: line, col: col,
    pos: pos, len: len, args: args, message: msg
  ))