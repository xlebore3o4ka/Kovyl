import ../core/[ast, types, tokens, errors]

type
  Context = ref object

proc visit*(ctx: Context, node: Expression)

proc setType(node: Expression, ctx: Context, exprType: Type) {.inline.} =
  node.exprType = exprType

proc isInteger(typ: TypeKind): bool {.inline.} = typ in {typeInt64}
proc isInteger(typ: Type):     bool {.inline.} = isInteger(typ.kind)

proc visitNumberExpression(ctx: Context, node: NumberExpression) =
  node.setType(ctx, getInt64Type())

proc visitBoolExpression(ctx: Context, node: BoolExpression) =
  node.setType(ctx, getBoolType())

proc visitUnaryExpression(ctx: Context, node: UnaryExpression) =
  ctx.visit(node.value)
  let op  = node.token.kind
  let typ = node.value.exprType

  if typ.isInteger() and op in {tkPlus, tkMinus}:
    node.setType(ctx, node.value.exprType)

  elif typ.eq(typeBool) and op == tkBang:
    node.setType(ctx, node.value.exprType)

  else:
    newError(errUnaryTypeMismatch, node.token, node.token.lexeme, typ)

proc visitBinaryExpression(ctx: Context, node: BinaryExpression) =
  ctx.visit(node.left)
  ctx.visit(node.right)

  block typeSemantics:
    if node.left.exprType.neq node.right.exprType:
      newError(errBinaryTypeMismatch, node.token, node.token.lexeme, node.left.exprType, node.right.exprType)
      break typeSemantics

    var typ = node.left.exprType
    let op  = node.token.kind

    block opSemantics:
      if   typ.isInteger() and op in {tkPlus, tkMinus, tkStar, tkSlash, tkPercent}: 
        break opSemantics
      elif typ.isInteger() and op in {tkGT, tkLT, tkGTE, tkLTE, tkEqualsEquals, tkBangEquals}: 
        typ = getBoolType()
        break opSemantics
      elif typ.eq(getBoolType()) and op in {tkAnd, tkOr, tkEqualsEquals, tkBangEquals}: 
        break opSemantics

      newError(errBinaryTypeMismatch, node.token, node.token.lexeme, node.left.exprType, node.right.exprType)
      break typeSemantics
    
    node.setType(ctx, typ)

proc visit(ctx: Context, node: Expression) =
  case node.kind:
    of exprInvalid: discard
    of exprNumber: visitNumberExpression(ctx, NumberExpression(node))
    of exprBool: visitBoolExpression(ctx, BoolExpression(node))
    of exprUnary: visitUnaryExpression(ctx, UnaryExpression(node))
    of exprBinary: visitBinaryExpression(ctx, BinaryExpression(node))

proc checkSemantics*(node: Expression) =
  new(Context)
    .visit(node)