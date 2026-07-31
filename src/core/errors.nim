import tokens

type
  ErrorKind* = enum
    errSyntaxChar, errSyntaxParenthesis

    errExpectedSyntax, errExpression, errStatement, errType

    errUnaryTypeMismatch, errBinaryTypeMismatch, errDeclarationTypeMismatch, errRedeclaration

  Error* = ref object
    kind*: ErrorKind
    file*: string
    line*: Positive
    col*: Positive
    len*: Positive
    args*: seq[string]
    message*: string

var errors*: seq[Error]

proc message(kind: ErrorKind): string =
  case kind:
  of errSyntaxChar:        "Syntax error unknown character: '@0'"
  of errSyntaxParenthesis: "@0 parenthesis"

  of errExpectedSyntax: "Expected @0, got @1"
  of errExpression:     "Invalid expression: @0"
  of errStatement:      "Invalid statement: @0"
  of errType:           "Invalid type: @0"

  of errUnaryTypeMismatch:       "Type mismatch for the unary operator @0: @1"
  of errBinaryTypeMismatch:      "Type mismatch for the binary operator @0: @1 @0 @2"
  of errDeclarationTypeMismatch: "Expected @0 for @1, got @2"
  of errRedeclaration:           "Redeclaration symbol '@0', originally declared in @1(@2:@3)"

proc newError*(kind: ErrorKind, file: string, line, col: Positive, len: Positive, args: varargs[string, `$`]) {.inline.} =
  errors.add(Error(
    kind: kind,
    file: file,
    line: line,
    col: col,
    len: len,
    args: @args,
    message: kind.message
  ))

proc newError*(kind: ErrorKind, token: Token, args: varargs[string, `$`]) {.inline.} =
  errors.add(Error(
    kind: kind,
    file: token.file,
    line: token.line,
    col: token.col,
    len: token.len,
    args: @args,
    message: kind.message
  ))