import ../core/[astnodes, types, errors, tokens]
import std/[strutils, logging, sequtils, tables]

proc isNumber*(t: Type): bool {.inline.} =
  t.kind in {typeInt64, typeInt32, typeInt16, typeInt8, typeUint64, typeUint32, typeUint16, typeUint8}

proc isValidInt*[T: SomeSignedInt](s: string): bool =
  try:
    let v = parseInt(s)
    return v >= low(T) and v <= high(T)
  except ValueError:
    return false

proc isValidUint*[T: SomeUnsignedInt](s: string): bool =
  try:
    let v = parseUInt(s)
    return v <= high(T)
  except ValueError:
    return false

proc inferNumberType*(node: NumberExpression, expected: Type): Type =
  if not expected.isNumber:
    return getInt64Type()
  let number = node.token.lexeme
  if expected.eq(typeInt8) and isValidInt[int8](number): return expected
  if expected.eq(typeInt16) and isValidInt[int16](number): return expected
  if expected.eq(typeInt32) and isValidInt[int32](number): return expected
  if expected.eq(typeInt64) and isValidInt[int64](number): return expected
  if expected.eq(typeUint8) and isValidUint[uint8](number): return expected
  if expected.eq(typeUint16) and isValidUint[uint16](number): return expected
  if expected.eq(typeUint32) and isValidUint[uint32](number): return expected
  if expected.eq(typeUint64) and isValidUint[uint64](number): return expected
  newError(errSize, node.token, @{"@0": number, "@1": $expected})
  return getInt64Type()

proc setType*(expr: Expression, returnType: Type) {.inline.} =
  expr.returnType = returnType
  info("Return type is set as: ", $returnType)

proc checkEqNeq*(node: BinaryExpression, expected: TypeKind): bool {.inline.} =
  if node.token.kind notin {tkEq, tkNeq}: return false
  node.left.returnType.kind.eq(expected) and node.right.returnType.kind.eq(expected)

proc checkEqNeqStrings*(node: BinaryExpression): bool {.inline.} =
  if node.token.kind notin {tkEq, tkNeq}: return false

  let dyn = getVecType(getCharType())
  let sta = getArrayType(getCharType(), 0)
  
  if node.left.returnType.eq(dyn) and node.right.returnType.eq(dyn): return true
  if node.left.returnType.eq(dyn) and node.right.returnType.eq(sta): return true
  if node.left.returnType.eq(sta) and node.right.returnType.eq(dyn): return true
  if node.left.returnType.eq(sta) and node.right.returnType.eq(sta): return true

  return false

proc checkAndOr*(node: BinaryExpression): bool {.inline.} =
  if node.token.kind notin {tkAnd, tkOr}: return false
  node.left.returnType.kind.eq(typeBool) and node.right.returnType.kind.eq(typeBool)

proc trySetNumber*(node: BinaryExpression): bool {.inline.} =
  if node.left.returnType.isNumber and node.right.returnType.eq node.left.returnType: 
    if node.token.kind in {tkPlus, tkMinus, tkStar, tkSlash, tkPercent}: 
      node.setType(node.left.returnType); return true
    elif node.token.kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}: 
      node.setType(getBoolType()); return true
  return false

proc trySetChar*(node: BinaryExpression): bool {.inline.} =
  if node.left.returnType.eq(typeChar) and node.right.returnType.eq node.left.returnType: 
    if node.token.kind in {tkPlus, tkMinus, tkStar, tkSlash, tkPercent}: 
      node.setType(node.left.returnType); return true
    elif node.token.kind in {tkGT, tkLT, tkGTE, tkLTE, tkEQ, tkNEQ}: 
      node.setType(getBoolType()); return true
  return false

proc newBinaryTypeMismatchError*(node: BinaryExpression) {.inline.} =
  newError(errBinaryTypeMismatch, node.token, @{"@0": node.token.lexeme, "@1": $node.left.returnType, 
      "@2": $node.right.returnType})

proc checkPlusMinus*(node: UnaryExpression): bool {.inline.} =
  if node.token.kind notin {tkPlus, tkMinus}: return false
  node.value.returnType.isNumber

proc checkNot*(node: UnaryExpression): bool {.inline.} =
  if node.token.kind != tkNot: return false
  node.value.returnType.eq getBoolType()

proc newUnaryTypeMismatchError*(node: UnaryExpression) {.inline.} =
  newError(errUnaryTypeMismatch, node.token, @{"@0": node.token.lexeme, "@1": $node.value.returnType})

proc blockEndsWithReturn*(node: Statement): bool =
  if node of ReturnStatement:
    return true
  elif node of BlockStatement:
    for stmt in BlockStatement(node).statements:
      if blockEndsWithReturn(stmt):
        return true
    return false
  elif node of BranchingStatement:
    let br = BranchingStatement(node)
    let ifEnds = blockEndsWithReturn(br.ifBlock)
    let elseEnds = br.elseBlock != nil and blockEndsWithReturn(br.elseBlock)
    
    var allElifsEnd = true
    for el in br.elifBlocks:
      if not blockEndsWithReturn(el.elifBlock):
        allElifsEnd = false
        break
    
    if br.elseBlock != nil and ifEnds and allElifsEnd and elseEnds:
      return true
    else:
      return false
  else:
    return false

proc formToString*(node: FormStatement): string =
  let strParams = node.formParams
    .mapIt(it.lexeme)
    .join(", ")
  let strArgs = node.arguments.values.toSeq
    .mapIt($it.expectedType)
    .join(", ")
  return node.name.lexeme & "<" & strParams & ">(" & strArgs & ") -> " & $node.returnType

proc cloneAst*(node: Expression): Expression =
  if node == nil: return nil
  
  if node of NumberExpression:
    result = newNumberExpression(node.token)

  elif node of BoolExpression:
    result = newBoolExpression(node.token)

  elif node of CharExpression:
    result = newCharExpression(node.token)

  elif node of NulExpression:
    result = newNulExpression(node.token)

  elif node of IdentifierExpression:
    result = newIdentifierExpression(node.token)

  elif node of TypeExpression:
    result = newTypeExpression(node.token, node.returnType)

  elif node of ErrorExpression:
    result = newErrorExpression(node.token)

  elif node of BinaryExpression:
    let n = BinaryExpression(node)
    result = newBinaryExpression(cloneAst(n.left), n.token, cloneAst(n.right))

  elif node of UnaryExpression:
    let n = UnaryExpression(node)
    result = newUnaryExpression(cloneAst(n.value), n.token)
  
  elif node of CastExpression:
    let n = CastExpression(node)
    result = newCastExpression(n.token, n.returnType, cloneAst(n.value))
 
  elif node of DerefExpression:
    let n = DerefExpression(node)
    result = newDerefExpression(n.token, cloneAst(n.value))
  
  elif node of ArrayExpression:
    let n = ArrayExpression(node)
    var arr = newArrayExpression(n.token)
    for v in n.values:
      arr.addExpr(cloneAst(v))
    result = arr
 
  elif node of IndexExpression:
    let n = IndexExpression(node)
    result = newIndexExpression(n.token, cloneAst(n.value), cloneAst(n.index))
 
  elif node of TupleExpression:
    let n = TupleExpression(node)
    var elems = initOrderedTable[Token, Expression]()
    for k, v in n.elements:
      elems[k] = cloneAst(v)
    result = newTupleExpression(n.token, elems)
 
  elif node of FieldExpression:
    let n = FieldExpression(node)
    result = newFieldExpression(cloneAst(n.value), n.token)
 
  elif node of CallExpression:
    let n = CallExpression(node)
    var args: seq[Expression] = @[]
    for a in n.arguments:
      args.add(cloneAst(a))
    result = newCallExpression(n.token, cloneAst(n.value), args)
    CallExpression(result).funcOverload = n.funcOverload
 
  elif node of SpecialExpression:
    let n = SpecialExpression(node)
    var args = initOrderedTable[Token, Expression]()
    for k, v in n.namedArgs:
      args[k] = cloneAst(v)
    result = newSpecialExpression(n.token, n.kind, args)
  
  elif node of InstanceExpression:
    let n = InstanceExpression(node)
    var ol = initOrderedTable[string, FuncStatement]()
    for k, v in n.overloads:
      ol[k] = v
    result = InstanceExpression(
      token: n.token,
      returnType: n.returnType,
      name: n.name,
      module: IdentifierExpression(cloneAst(n.module)),
      types: n.types,
      overloads: ol
    )
  
  result.returnType = node.returnType
  result.token = node.token

proc cloneAst*(node: Statement): Statement =
  if node == nil: return nil
  
  if node of BlockStatement:
    let n = BlockStatement(node)
    result = newBlockStatement(n.startToken, n.endToken)
    for s in n.statements:
      BlockStatement(result).addStatement(cloneAst(s))
 
  elif node of DeclarationStatement:
    let n = DeclarationStatement(node)
    result = newDeclarationStatement(n.symbolType, n.name, cloneAst(n.value), n.pub)
 
  elif node of AssignmentStatement:
    let n = AssignmentStatement(node)
    result = newAssignmentStatement(cloneAst(n.left), cloneAst(n.value))
 
  elif node of ErrorStatement:
    let n = ErrorStatement(node)
    result = newErrorStatement(n.token)
 
  elif node of BranchingStatement:
    let n = BranchingStatement(node)
    result = newBranchingStatement(cloneAst(n.condition), BlockStatement(cloneAst(n.ifBlock)))
    for el in n.elifBlocks:
      BranchingStatement(result).addElif(cloneAst(el.cond), BlockStatement(cloneAst(el.elifBlock)))
    if n.elseBlock != nil:
      BranchingStatement(result).setElse(BlockStatement(cloneAst(n.elseBlock)))

  elif node of WhileStatement:
    let n = WhileStatement(node)
    result = newWhileStatement(n.token, cloneAst(n.condition), BlockStatement(cloneAst(n.whileBlock)))
 
  elif node of BreakStatement:
    result = newBreakStatement(BreakStatement(node).token)
 
  elif node of ContinueStatement:
    result = newContinueStatement(ContinueStatement(node).token)
 
  elif node of DefaultStatement:
    let n = DefaultStatement(node)
    result = newDefaultStatement(n.symbolType, n.name, n.pub)

  elif node of FuncStatement:
    let n = FuncStatement(node)
    var args = initOrderedTable[string, FuncArgument]()
    for k, v in n.arguments:
      args[k] = v
    result = newFuncStatement(n.returnType, n.name, args, BlockStatement(cloneAst(n.funcBlock)), n.pub)
    FuncStatement(result).funcType = n.funcType
    FuncStatement(result).funcClosures = n.funcClosures
  
  elif node of ReturnStatement:
    let n = ReturnStatement(node)
    if n.hasValue:
      result = newReturnStatement(n.token, true, cloneAst(n.value))
    else:
      result = newReturnStatement(n.token, false)
 
  elif node of ForStatement:
    let n = ForStatement(node)
    result = newForStatement(n.token, n.name, cloneAst(n.value), BlockStatement(cloneAst(n.forBlock)))
 
  elif node of CallStatement:
    let n = CallStatement(node)
    result = newCallStatement(CallExpression(cloneAst(n.callExpression)))
 
  elif node of ModuleStatement:
    let n = ModuleStatement(node)
    result = newModuleStatement(n.name, n.path)
    ModuleStatement(result).moduleBlock = BlockStatement(cloneAst(n.moduleBlock))
    ModuleStatement(result).moduleType = n.moduleType
 
  elif node of ClosureStatement:
    let n = ClosureStatement(node)
    result = newClosureStatement(n.token, n.names)

  elif node of SpecialStatement:
    let n = SpecialStatement(node)
    var args = initOrderedTable[Token, Expression]()
    for k, v in n.namedArgs:
      args[k] = cloneAst(v)
    result = newSpecialStatement(n.token, n.kind, args)
 
  elif node of FormStatement:
    let n = FormStatement(node)
    var args = initOrderedTable[string, FuncArgument]()
    for k, v in n.arguments:
      args[k] = v
    result = newFormStatement(n.returnType, n.name, args, BlockStatement(cloneAst(n.formBlock)), n.formParams, n.pub)

proc recursiveMonomorphization*(node: Statement, typeMap: Table[string, Type])

proc recursiveMonomorphization*(node: Expression, typeMap: Table[string, Type]) =
  if node == nil: return
  
  if node.returnType != nil and node.returnType.kind == typeVar:
    if node.returnType.varName in typeMap:
      node.returnType = typeMap[node.returnType.varName]
  
  if node of BinaryExpression:
    let n = BinaryExpression(node)
    recursiveMonomorphization(n.left, typeMap)
    recursiveMonomorphization(n.right, typeMap)
  
  elif node of UnaryExpression:
    let n = UnaryExpression(node)
    recursiveMonomorphization(n.value, typeMap)
  
  elif node of CastExpression:
    let n = CastExpression(node)
    recursiveMonomorphization(n.value, typeMap)
    if n.returnType != nil and n.returnType.kind == typeVar and n.returnType.varName in typeMap:
      n.returnType = typeMap[n.returnType.varName]
  
  elif node of DerefExpression:
    let n = DerefExpression(node)
    recursiveMonomorphization(n.value, typeMap)
  
  elif node of ArrayExpression:
    let n = ArrayExpression(node)
    for v in n.values:
      recursiveMonomorphization(v, typeMap)
  
  elif node of IndexExpression:
    let n = IndexExpression(node)
    recursiveMonomorphization(n.value, typeMap)
    recursiveMonomorphization(n.index, typeMap)
  
  elif node of TupleExpression:
    let n = TupleExpression(node)
    for _, v in n.elements:
      recursiveMonomorphization(v, typeMap)
  
  elif node of FieldExpression:
    let n = FieldExpression(node)
    recursiveMonomorphization(n.value, typeMap)
  
  elif node of CallExpression:
    let n = CallExpression(node)
    recursiveMonomorphization(n.value, typeMap)
    for a in n.arguments:
      recursiveMonomorphization(a, typeMap)
  
  elif node of SpecialExpression:
    let n = SpecialExpression(node)
    for _, v in n.namedArgs:
      recursiveMonomorphization(v, typeMap)
  
  elif node of InstanceExpression:
    let n = InstanceExpression(node)
    for i, t in n.types:
      if t.kind == typeVar and t.varName in typeMap:
        n.types[i] = typeMap[t.varName]
    for _, v in n.overloads:
      recursiveMonomorphization(v, typeMap)

proc recursiveMonomorphization*(node: Statement, typeMap: Table[string, Type]) =
  if node == nil: return
  
  if node of BlockStatement:
    let n = BlockStatement(node)
    for s in n.statements:
      recursiveMonomorphization(s, typeMap)
  
  elif node of DeclarationStatement:
    let n = DeclarationStatement(node)
    if n.symbolType != nil:
      for varName, replacement in typeMap:
        n.symbolType = substituteTypeVar(n.symbolType, varName, replacement)
    recursiveMonomorphization(n.value, typeMap)
  
  elif node of AssignmentStatement:
    let n = AssignmentStatement(node)
    recursiveMonomorphization(n.left, typeMap)
    recursiveMonomorphization(n.value, typeMap)
  
  elif node of BranchingStatement:
    let n = BranchingStatement(node)
    recursiveMonomorphization(n.condition, typeMap)
    recursiveMonomorphization(n.ifBlock, typeMap)
    for el in n.elifBlocks:
      recursiveMonomorphization(el.cond, typeMap)
      recursiveMonomorphization(el.elifBlock, typeMap)
    if n.elseBlock != nil:
      recursiveMonomorphization(n.elseBlock, typeMap)
  
  elif node of WhileStatement:
    let n = WhileStatement(node)
    recursiveMonomorphization(n.condition, typeMap)
    recursiveMonomorphization(n.whileBlock, typeMap)
  
  elif node of FuncStatement:
    var n = FuncStatement(node)
    if n.returnType != nil:
      for varName, replacement in typeMap:
        n.returnType = substituteTypeVar(n.returnType, varName, replacement)
    for _, arg in n.arguments:
      if arg.expectedType != nil:
        for varName, replacement in typeMap:
          arg.expectedType = substituteTypeVar(arg.expectedType, varName, replacement)
    recursiveMonomorphization(n.funcBlock, typeMap)
  
  elif node of ReturnStatement:
    let n = ReturnStatement(node)
    if n.hasValue:
      recursiveMonomorphization(n.value, typeMap)
  
  elif node of ForStatement:
    let n = ForStatement(node)
    recursiveMonomorphization(n.value, typeMap)
    recursiveMonomorphization(n.forBlock, typeMap)
  
  elif node of CallStatement:
    let n = CallStatement(node)
    recursiveMonomorphization(n.callExpression, typeMap)
  
  elif node of ModuleStatement:
    let n = ModuleStatement(node)
    recursiveMonomorphization(n.moduleBlock, typeMap)
  
  elif node of FormStatement:
    var n = FormStatement(node)
    if n.returnType != nil:
      for varName, replacement in typeMap:
        n.returnType = substituteTypeVar(n.returnType, varName, replacement)
    for _, arg in n.arguments:
      if arg.expectedType != nil:
        for varName, replacement in typeMap:
          arg.expectedType = substituteTypeVar(arg.expectedType, varName, replacement)
    recursiveMonomorphization(n.formBlock, typeMap)