import ../core/[ast, tokens, types]
import std/[sets, strutils]
import codegenSanitizers

type
  Context = ref object
    includes: HashSet[string]
    code: string

    nameCounter: Natural

proc emit(ctx: Context, code: varargs[string, `$`]) =
  ctx.code &= code.join("") & "\n"

proc ctype(ctx: Context, typ: Type): string =
  case typ.kind:
  of typeUndefined: return "void"
  of typeInt64: return "int64_t"
  of typeBool: return "bool"

proc generateUniqueName(ctx: Context, prefix: string): string =
  result = "_" & prefix & "_" & $ctx.nameCounter
  ctx.nameCounter.inc

proc visit(ctx: Context, node: Expression): string
proc visit(ctx: Context, node: Statement)

proc visitNumberExpression(ctx: Context, node: NumberExpression): string =
  return node.token.lexeme

proc visitBoolExpression(ctx: Context, node: BoolExpression): string =
  return node.token.lexeme

proc visitUnaryExpression(ctx: Context, node: UnaryExpression): string =
  return node.token.lexeme & ctx.visit(node.value)

proc visitBinaryExpression(ctx: Context, node: BinaryExpression): string =
  var op = node.token.lexeme

  let left = ctx.visit(node.left)
  var right = ctx.visit(node.right)

  if   node.token.kind == tkAnd: op = "&&"
  elif node.token.kind == tkOr:  op = "||"
  elif node.token.kind == tkSlash:
    let name = ctx.generateUniqueName("temp")
    ctx.emit(ctx.ctype(node.right.exprType), " ", name, " = ", right, ";")
    right = name
    ctx.emit(generateZeroDivisionSanitizer(right, node.right.token.line))

  return "(" & left & " " & op & " " & right & ")"


# STATEMENTS


proc visitBlockStatement(ctx: Context, node: BlockStatement) =
  for stmt in node.statements:
    ctx.visit(stmt)

proc visitDeclarationStatement(ctx: Context, node: DeclarationStatement) =
  ctx.emit(ctx.ctype(node.valueType), " ", ctx.generateUniqueName(node.name.lexeme), " = ", ctx.visit(node.value), ";")

proc visit(ctx: Context, node: Expression): string =
  case node.kind:
  of exprNumber: return visitNumberExpression(ctx, NumberExpression(node))
  of exprBool: return visitBoolExpression(ctx, BoolExpression(node))
  of exprUnary: return visitUnaryExpression(ctx, UnaryExpression(node))
  of exprBinary: return visitBinaryExpression(ctx, BinaryExpression(node))
  else: discard

proc visit(ctx: Context, node: Statement) =
  case node.kind:
  of stmtBlock: visitBlockStatement(ctx, BlockStatement(node))
  of stmtDeclaration: visitDeclarationStatement(ctx, DeclarationStatement(node))
  else: discard

proc generate*(node: Statement, release: bool = false): string =
  let ctx = Context(includes: ["<stdbool.h>", "<stdint.h>", "<stdio.h>", "<stdlib.h>"].toHashSet)

  ctx.emit(generateDebugDefine(release))

  ctx.emit(generatePanicSystem("kovypanic"))

  ctx.emit("int main() {")
  ctx.visit(node)
  ctx.emit("}")
  
  for name in ctx.includes:
    ctx.code = "#include " & name & "\n" & ctx.code

  return ctx.code
