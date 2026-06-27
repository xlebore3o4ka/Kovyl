import tokens

type
  ErrorKind* = enum
    errSyntax, errExpression, errStatement, errExpectedSyntax

  CompileError* = ref object
    kind*: ErrorKind
    file*: string
    line*: Natural
    col*: Natural
    pos*: Positive
    len*: Natural
    args*: seq[(string, string)]
    message*: string

var errors*: seq[CompileError] = @[]

proc message(kind: ErrorKind): string =
  case kind
    of errSyntax: "Invalid syntax"
    of errExpression: "Expected expression, got @0"
    of errStatement: "Expected statement, got @0"
    of errExpectedSyntax: "Expected @0, got @1"

proc newError*(
              kind: ErrorKind, file: string, token: Token, 
              args: seq[(string, string)] = @[]) =
  let msg = kind.message()
  
  errors.add(CompileError(
    kind: kind, file: token.file, line: token.line, col: token.column,
    pos: token.offset, len: token.lexeme.len, args: args, message: msg
  ))

proc newError*(
              kind: ErrorKind, file: string, line: Natural, col: Natural, 
              pos: Positive, len: Natural, 
              args: seq[(string, string)] = @[]) =
  let msg = kind.message()
  
  errors.add(CompileError(
    kind: kind, file: file, line: line, col: col,
    pos: pos, len: len, args: args, message: msg
  ))