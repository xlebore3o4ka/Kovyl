import ../core/[ast, types, tokens, errors]
import std/[tables]

type
  Symbol = object
    definitionToken: Token
    symbolType: Type

  Scope = ref object
    depth: Natural
    symbolTable: Table[string, Symbol]
    case isGlobal: bool
    of false: parent: Scope
    of true: discard

  Context = ref object
    currentScope: Scope
    symbolScopeStack: Table[string, seq[Scope]]

proc newSymbol(self: Context, name: Token, symbolType: Type) =
  self.currentScope.symbolTable[name.lexeme] = Symbol(definitionToken: name, symbolType: symbolType)
  self.symbolScopeStack.mgetOrPut(name.lexeme, @[]).add(self.currentScope)

proc pushScope(self: Context) =
  let depth = self.currentScope.depth
  self.currentScope = Scope(isGlobal: false, symbolTable: initTable[string, Symbol](), 
    parent: self.currentScope, depth: depth + 1)

proc popScope(self: Context) =
  let scope = self.currentScope
  for name in scope.symbolTable.keys:
    discard self.symbolScopeStack[name].pop()
  self.currentScope = scope.parent

proc getSymbol(self: Context, name: string): Symbol =
  let scope = self.symbolScopeStack[name][^1]
  result = scope.symbolTable[name]

proc symbolExists(self: Context, name: string): bool =
  let exists = name in self.symbolScopeStack and self.symbolScopeStack[name].len > 0
  return exists

proc symbolExistsInCurrentScope(self: Context, name: string): bool =
  let exists = name in self.currentScope.symbolTable
  return exists

proc visit(ctx: Context, node: Expression)
proc visit(ctx: Context, node: Statement)

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


# STATEMENTS


proc visitBlockStatement(ctx: Context, node: BlockStatement) =
  for stmt in node.statements:
    ctx.visit(stmt)

proc visitDeclarationStatement(ctx: Context, node: DeclarationStatement) =
  ctx.visit(node.value)

  block semantics:
    if node.value.exprType.neq node.valueType:
      newError(errDeclarationTypeMismatch, node.token, node.valueType, node.name.lexeme, node.value.exprType)
      break semantics

    if ctx.symbolExistsInCurrentScope(node.name.lexeme):
      let symbol = ctx.getSymbol(node.name.lexeme)
      let symbolToken = symbol.definitionToken
      newError(errRedeclaration, node.name, symbolToken.lexeme, symbolToken.file, symbolToken.line, symbolToken.col)
      break semantics

    ctx.newSymbol(node.name, node.valueType)

proc visit(ctx: Context, node: Expression) =
  case node.kind:
  of exprNumber: visitNumberExpression(ctx, NumberExpression(node))
  of exprBool: visitBoolExpression(ctx, BoolExpression(node))
  of exprUnary: visitUnaryExpression(ctx, UnaryExpression(node))
  of exprBinary: visitBinaryExpression(ctx, BinaryExpression(node))
  else: discard

proc visit(ctx: Context, node: Statement) =
  case node.kind:
  of stmtBlock: visitBlockStatement(ctx, BlockStatement(node))
  of stmtDeclaration: visitDeclarationStatement(ctx, DeclarationStatement(node))
  else: discard

proc checkSemantics*(node: Statement) =
  var ctx = Context(
    currentScope: Scope(
      depth: 0,
      isGlobal: true,
      symbolTable: initTable[string, Symbol]()
    ),
    symbolScopeStack: initTable[string, seq[Scope]]()
  )
  ctx.visit(node)