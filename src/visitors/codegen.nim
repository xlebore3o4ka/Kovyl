import ../core/[ast, tokens, types]
import std/[sets, strutils, sequtils]
import codegen/[sanitizers, runtime]

const TAB_WIDTH = 2

type
  Context = ref object
    includes: HashSet[string]
    code: string
    globalCode: string
    emitToGlobal: bool

    indent: Natural
    nameCounter: Natural

proc prepend(code: var string, text: string) =
  code = text & code

proc emit(ctx: Context, code: varargs[string, `$`]) =
  let joined = code.join("")
  var target = if ctx.emitToGlobal: ctx.globalCode else: ctx.code
  
  for line in joined.splitLines():
    if line.len > 0:
      target &= " ".repeat(ctx.indent) & line & "\n"
    else:
      target &= "\n"

  if ctx.emitToGlobal: ctx.globalCode = target
  else: ctx.code = target

proc ctype(ctx: Context, typ: Type): string =
  case typ.kind:
  of typeUndefined: return "void"
  of typeInt64: return "int64_t"
  of typeBool: return "bool"
  of typeFunc: echo "ctype unsupported type"; return "void /*func*/"

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
  elif node.token.kind in {tkSlash, tkPercent}:
    let name = ctx.generateUniqueName("temp")
    ctx.emit(ctx.ctype(node.right.exprType), " ", name, " = ", right, ";")
    right = name
    ctx.emit(generateZeroDivisionSanitizer(right, node.right.token.line))

  return "(" & left & " " & op & " " & right & ")"

proc visitIdentExpression(ctx: Context, node: IdentExpression): string =
  node.token.lexeme

proc visitCallExpression(ctx: Context, node: CallExpression): string =
  result = ctx.visit(node.value) & "(" & 
    node.args.mapIt(ctx.visit(it)).join(", ") & ")"


# STATEMENTS


proc visitBlockStatement(ctx: Context, node: BlockStatement) =
  ctx.indent += TAB_WIDTH
  for stmt in node.statements:
    ctx.visit(stmt)
  ctx.indent -= TAB_WIDTH

proc visitDeclarationStatement(ctx: Context, node: DeclarationStatement) =
  ctx.emit(ctx.ctype(node.valueType), " ", node.name.lexeme, " = ", ctx.visit(node.value), ";")

proc visitAssignmentStatement(ctx: Context, node: AssignmentStatement) =
  ctx.emit(ctx.visit(node.left), " = ", ctx.visit(node.right), ";")

proc visitBranchingStatement(ctx: Context, node: BranchingStatement) =
  ctx.emit("if (", ctx.visit(node.condition), ") {")
  ctx.visit(node.ifBlock)
  ctx.emit("}")
  
  for elifBranch in node.elifBranches:
    ctx.emit("else if (", ctx.visit(elifBranch.cond), ") {")
    ctx.visit(elifBranch.elifBlock)
    ctx.emit("}")
  
  if node.elseBlock != nil:
    ctx.emit("else {")
    ctx.visit(node.elseBlock)
    ctx.emit("}")

proc visitWhileStatement(ctx: Context, node: WhileStatement) =
  ctx.emit("while (", ctx.visit(node.condition), ") {")
  ctx.visit(node.whileBlock)
  ctx.emit("}")

proc visitContinueStatement(ctx: Context, node: ContinueStatement) =
  ctx.emit("continue;")

proc visitBreakStatement(ctx: Context, node: BreakStatement) =
  ctx.emit("break;")

proc visitFuncStatement(ctx: Context, node: FuncStatement) =
  ctx.indent -= TAB_WIDTH
  ctx.emitToGlobal = true

  ctx.emit(ctx.ctype(node.returnType), " ", node.name.lexeme, "(",
    node.args.mapIt(ctx.ctype(it.argType) & " " & it.argToken.lexeme).join(", "),
    ") {"
  )
  ctx.visit(node.funcBlock)
  ctx.emit("}")

  ctx.emitToGlobal = false
  ctx.indent += TAB_WIDTH

proc visitReturnStatement(ctx: Context, node: ReturnStatement) =
  ctx.emit("return ", (if node.value != nil: ctx.visit(node.value) else: ""), ";")

proc visit(ctx: Context, node: Expression): string =
  case node.kind:
  of exprNumber: return visitNumberExpression(ctx, NumberExpression(node))
  of exprBool: return visitBoolExpression(ctx, BoolExpression(node))
  of exprUnary: return visitUnaryExpression(ctx, UnaryExpression(node))
  of exprBinary: return visitBinaryExpression(ctx, BinaryExpression(node))
  of exprIdent: return visitIdentExpression(ctx, IdentExpression(node))
  of exprCall: return visitCallExpression(ctx, CallExpression(node))
  else: discard

proc visit(ctx: Context, node: Statement) =
  case node.kind:
  of stmtBlock: visitBlockStatement(ctx, BlockStatement(node))
  of stmtDeclaration: visitDeclarationStatement(ctx, DeclarationStatement(node))
  of stmtAssignment: visitAssignmentStatement(ctx, AssignmentStatement(node))
  of stmtBranching: visitBranchingStatement(ctx, BranchingStatement(node))
  of stmtWhile: visitWhileStatement(ctx, WhileStatement(node))
  of stmtContinue: visitContinueStatement(ctx, ContinueStatement(node))
  of stmtBreak: visitBreakStatement(ctx, BreakStatement(node))
  of stmtFunc: visitFuncStatement(ctx, FuncStatement(node))
  of stmtReturn: visitReturnStatement(ctx, ReturnStatement(node))
  else: discard

proc generate*(node: Statement, release: bool = false): string =
  let ctx = Context(includes: ["<stdlib.h>"].toHashSet)

  for incl in sanitizerIncludes() + runtimeIncludes():
    ctx.includes.incl(incl)

  ctx.emit("int main() {")
  ctx.visit(node)
  ctx.emit("}")

  ctx.code.prepend(ctx.globalCode & "\n")
  ctx.code.prepend(generatePanicSystem())
  ctx.code.prepend(generateDebugDefine(release))
  
  ctx.code.prepend(generateRuntime(ctx.includes))

  return ctx.code
