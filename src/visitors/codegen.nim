import ../core/[ast, tokens]

type
  Context = ref object

proc visit*(ctx: Context, node: Expression): string

proc visitNumberExpression(ctx: Context, node: NumberExpression): string =
  return node.token.lexeme

proc visitBoolExpression(ctx: Context, node: BoolExpression): string =
  return node.token.lexeme

proc visitUnaryExpression(ctx: Context, node: UnaryExpression): string =
  return node.token.lexeme & ctx.visit(node.value)

proc visitBinaryExpression(ctx: Context, node: BinaryExpression): string =
  var op = node.token.lexeme
  if   node.token.kind == tkAnd: op = "&&"
  elif node.token.kind == tkOr:  op = "||"
  return "(" & ctx.visit(node.left) & " " & op & " " & ctx.visit(node.right) & ")"

proc visit(ctx: Context, node: Expression): string =
  case node.kind:
  of exprInvalid: return "0 /*error*/"
  of exprNumber: return visitNumberExpression(ctx, NumberExpression(node))
  of exprBool: return visitBoolExpression(ctx, BoolExpression(node))
  of exprUnary: return visitUnaryExpression(ctx, UnaryExpression(node))
  of exprBinary: return visitBinaryExpression(ctx, BinaryExpression(node))

proc generate*(node: Expression): string =
  let ctx = new(Context)
  return ctx.visit(node)
