import tokens

type
  ErrorKind* = enum
    errSyntax, errExpression, errStatement, errExpectedSyntax, errCannotAssign
    errForbiddenLocation, errNumberLiteral

    errMismatchedBracket, errUnexpectedBracket, errUnclosedBracket, 
    errUnclosedString, errUnclosedChar, errEmptyCharLiteral

    errBinaryTypeMismatch, errUnaryTypeMismatch, errTypeMismatch, 
    errUnknownType, errCannotCast, errProhibitedType

    errRedeclaration, errUndeclaredSymbol, errStmtSpecial, errExprSpecial

    errUnknownPragma

    errUnexpectedArgument, errUnexpectedNamedArgument, errMissingArgument

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
    of errCannotAssign: "Cannot assign to this expression"
    of errForbiddenLocation: "The statement is in the forbidden location"
    of errNumberLiteral: "Number literal '@0' does not fit in type @1"
    of errMismatchedBracket: "Mismatched bracket"
    of errUnexpectedBracket: "Unexpected closing bracket"
    of errUnclosedBracket: "Unclosed bracket"
    of errUnclosedString: "Unclosed string literal"
    of errUnclosedChar: "Unclosed character literal"
    of errEmptyCharLiteral: "Empty character literal"
    of errBinaryTypeMismatch: "Type mismatch for binary operator '@0' (@1 @0 @2)"
    of errUnaryTypeMismatch: "Type mismatch for unary operator '@0' (@1)"
    of errTypeMismatch: "Type mismatch for @0 (expected @0, got @1)"
    of errUnknownType: "Unknown type"
    of errCannotCast: "Cannot cast from @0 to @1"
    of errProhibitedType: "@0 is a prohibited type in this construction"
    of errRedeclaration: "Redeclaration of symbol '@0', originally declared at @1(@2:@3)"
    of errUndeclaredSymbol: "Undeclared symbol '@0'"
    of errStmtSpecial: "Unknown special statement"
    of errExprSpecial: "Unknown special expression"
    of errUnknownPragma: "Unknown pragma"
    of errUnexpectedNamedArgument: "Unexpected named argument '@0'"
    of errUnexpectedArgument: "Unexpected argument at position @0"
    of errMissingArgument: "Missing required argument '@0'"

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