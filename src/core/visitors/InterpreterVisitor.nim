import visitor
import ../[astnodes, types, tokens]
import std/[strutils, tables, math]

type
  RuntimeError* = object of CatchableError

  ArrayValue* = ref object
    elements*: seq[Value]
    length*: Value

  Value* = object
    valueType*: Type
    case valueTypeKind*: TypeKind
    of typeUndefined: discard
    of typeInt64: int64Value*: int64
    of typeInt32: int32Value*: int32
    of typeInt16: int16Value*: int16
    of typeInt8: int8Value*: int8
    of typeUint64: uint64Value*: uint64
    of typeUint32: uint32Value*: uint32
    of typeUint16: uint16Value*: uint16
    of typeUint8: uint8Value*: uint8
    of typeBool: boolValue*: bool
    of typePtr, typeNul: ptrValue*: ref Value
    of typeChar: charValue*: char
    of typeArray: arrayValue*: ArrayValue

  InterpreterVisitor* = ref object of Visitor
    valueScopes*: seq[Table[string, Value]] = @[]

  BreakException* = object of CatchableError
  ContinueException* = object of CatchableError

proc pushScope(visitor: InterpreterVisitor) =
  visitor.valueScopes.add(initTable[string, Value]())

proc popScope(visitor: InterpreterVisitor) =
  discard visitor.valueScopes.pop()

proc getValue(visitor: InterpreterVisitor, name: string): Value =
  for i in countdown(visitor.valueScopes.len - 1, 0):
    if visitor.valueScopes[i].hasKey(name):
      return visitor.valueScopes[i][name]
  raise newException(RuntimeError, "Undefined variable: " & name)

proc setValue(visitor: InterpreterVisitor, name: string, value: Value) =
  for i in countdown(visitor.valueScopes.len - 1, 0):
    if visitor.valueScopes[i].hasKey(name):
      visitor.valueScopes[i][name] = value
      return
      
  visitor.valueScopes[^1][name] = value

proc newInterpreterVisitor*(): InterpreterVisitor =
  result = InterpreterVisitor()
  result.pushScope()

proc `==`*(a, b: Value): bool =
  if a.valueTypeKind == typeNul and b.valueTypeKind == typePtr:
    return b.ptrValue[].valueTypeKind == typeNul
  if a.valueTypeKind == typePtr and b.valueTypeKind == typeNul:
    return a.ptrValue[].valueTypeKind == typeNul
  if a.valueTypeKind == typeNul and b.valueTypeKind == typeArray:
    return b.arrayValue.elements.len == 0
  if a.valueTypeKind == typeArray and b.valueTypeKind == typeNul:
    return a.arrayValue.elements.len == 0

  if a.valueType != b.valueType:
    return false
  if a.valueTypeKind != b.valueTypeKind:
    return false
  
  case a.valueTypeKind
  of typeInt8: return a.int8Value == b.int8Value
  of typeInt16: return a.int16Value == b.int16Value
  of typeInt32: return a.int32Value == b.int32Value
  of typeInt64: return a.int64Value == b.int64Value
  of typeUint8: return a.uint8Value == b.uint8Value
  of typeUint16: return a.uint16Value == b.uint16Value
  of typeUint32: return a.uint32Value == b.uint32Value
  of typeUint64: return a.uint64Value == b.uint64Value
  of typeBool: return a.boolValue == b.boolValue
  of typeChar: return a.charValue == b.charValue
  of typePtr: return a.ptrValue == b.ptrValue
  of typeArray: return a.arrayValue == b.arrayValue
  of typeNul: return true
  else: 
    raise newException(RuntimeError, "The type '" & $a.valueType & "' cannot be compared")

proc `==`*(a, b: ArrayValue): bool =
  if a.elements.len != b.elements.len:
    return false
  for i in 0..<a.elements.len:
    if a.elements[i] != b.elements[i]:
      return false
  return true

proc newInt8Value*(v: int8): Value = 
  Value(valueTypeKind: typeInt8, valueType: getInt8Type(), int8Value: v)
proc newInt16Value*(v: int16): Value = 
  Value(valueTypeKind: typeInt16, valueType: getInt16Type(), int16Value: v)
proc newInt32Value*(v: int32): Value = 
  Value(valueTypeKind: typeInt32, valueType: getInt32Type(), int32Value: v)
proc newInt64Value*(v: int64): Value = 
  Value(valueTypeKind: typeInt64, valueType: getInt64Type(), int64Value: v)
proc newUint8Value*(v: uint8): Value = 
  Value(valueTypeKind: typeUint8, valueType: getUint8Type(), uint8Value: v)
proc newUint16Value*(v: uint16): Value = 
  Value(valueTypeKind: typeUint16, valueType: getUint16Type(), uint16Value: v)
proc newUint32Value*(v: uint32): Value = 
  Value(valueTypeKind: typeUint32, valueType: getUint32Type(), uint32Value: v)
proc newUint64Value*(v: uint64): Value = 
  Value(valueTypeKind: typeUint64, valueType: getUint64Type(), uint64Value: v)
proc newBoolValue*(v: bool): Value = 
  Value(valueTypeKind: typeBool, valueType: getBoolType(), boolValue: v)
proc newCharValue*(v: char): Value = 
  Value(valueTypeKind: typeChar, valueType: getCharType(), charValue: v)
proc newPtrValue*(v: ref Value, baseType: Type): Value = 
  Value(valueTypeKind: typePtr, valueType: getPtrType(baseType), ptrValue: v)
proc newArrayValue*(elements: seq[Value], baseType: Type): Value =
  Value(valueTypeKind: typeArray, valueType: getArrayType(baseType), 
    arrayValue: ArrayValue(elements: elements, length: newUint64Value(uint64(elements.len))))
proc newNulValue*(): Value = 
  Value(valueTypeKind: typeNul, valueType: getNulType())

proc intValue*(v: Value): int =
  case v.valueTypeKind:
  of typeInt8: return int(v.int8Value)
  of typeInt16: return int(v.int16Value)
  of typeInt32: return int(v.int32Value)
  of typeInt64: return int(v.int64Value)
  of typeUint8: return int(v.uint8Value)
  of typeUint16: return int(v.uint16Value)
  of typeUint32: return int(v.uint32Value)
  of typeUint64: return int(v.uint64Value)
  else: raise newException(RuntimeError, "Value is not numeric")

proc uintValue*(v: Value): uint =
  case v.valueTypeKind:
  of typeInt8: return uint(v.int8Value)
  of typeInt16: return uint(v.int16Value)
  of typeInt32: return uint(v.int32Value)
  of typeInt64: return uint(v.int64Value)
  of typeUint8: return uint(v.uint8Value)
  of typeUint16: return uint(v.uint16Value)
  of typeUint32: return uint(v.uint32Value)
  of typeUint64: return uint(v.uint64Value)
  else: raise newException(RuntimeError, "Value is not numeric")

method visitExpression*(visitor: InterpreterVisitor, node: Expression): Value {.base.}
method visitStatement*(visitor: InterpreterVisitor, node: Statement) {.base.}

method visitNumberExpression*(visitor: InterpreterVisitor, node: NumberExpression): Value {.base.} =
  case node.returnType.kind:
  of typeInt8: return newInt8Value(parseInt(node.token.lexeme).int8)
  of typeInt16: return newInt16Value(parseInt(node.token.lexeme).int16)
  of typeInt32: return newInt32Value(parseInt(node.token.lexeme).int32)
  of typeInt64: return newInt64Value(parseInt(node.token.lexeme).int64)
  of typeUint8: return newUint8Value(parseUint(node.token.lexeme).uint8)
  of typeUint16: return newUint16Value(parseUint(node.token.lexeme).uint16)
  of typeUint32: return newUint32Value(parseUint(node.token.lexeme).uint32)
  of typeUint64: return newUint64Value(parseUint(node.token.lexeme).uint64)
  else: raise newException(RuntimeError, "Unknown number type: " & $node.returnType)

method visitBoolExpression*(visitor: InterpreterVisitor, node: BoolExpression): Value {.base.} =
  return newBoolValue(node.token.kind == tkTrue)

method visitBinaryExpression*(visitor: InterpreterVisitor, node: BinaryExpression): Value {.base.} =
  let leftVal = visitor.visitExpression(node.left)
  let rightVal = visitor.visitExpression(node.right)

  case node.token.kind:
  of tkPlus:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(leftVal.int64Value + rightVal.int64Value)
    of typeUint64: return newUint64Value(leftVal.uint64Value + rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkMinus:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(leftVal.int64Value - rightVal.int64Value)
    of typeUint64: return newUint64Value(leftVal.uint64Value - rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkStar:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(leftVal.int64Value * rightVal.int64Value)
    of typeUint64: return newUint64Value(leftVal.uint64Value * rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkSlash:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(leftVal.int64Value div rightVal.int64Value)
    of typeUint64: return newUint64Value(leftVal.uint64Value div rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkPercent:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(((leftVal.int64Value mod rightVal.int64Value) + rightVal.int64Value) mod rightVal.int64Value)
    of typeUint64: return newUint64Value(((leftVal.uint64Value mod rightVal.uint64Value) + rightVal.uint64Value) mod rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkEQ:
    return newBoolValue(leftVal == rightVal)
  of tkNEQ:
    return newBoolValue(leftVal != rightVal)
  of tkGT:
    case node.left.returnType.kind:
    of typeInt64: return newBoolValue(leftVal.int64Value > rightVal.int64Value)
    of typeUint64: return newBoolValue(leftVal.uint64Value > rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkLT:
    case node.left.returnType.kind:
    of typeInt64: return newBoolValue(leftVal.int64Value < rightVal.int64Value)
    of typeUint64: return newBoolValue(leftVal.uint64Value < rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkGTE:
    case node.left.returnType.kind:
    of typeInt64: return newBoolValue(leftVal.int64Value >= rightVal.int64Value)
    of typeUint64: return newBoolValue(leftVal.uint64Value >= rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkLTE:
    case node.left.returnType.kind:
    of typeInt64: return newBoolValue(leftVal.int64Value <= rightVal.int64Value)
    of typeUint64: return newBoolValue(leftVal.uint64Value <= rightVal.uint64Value)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkAnd:
    case node.returnType.kind:
    of typeBool: return newBoolValue(leftVal.boolValue and rightVal.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkOr:
    case node.returnType.kind:
    of typeBool: return newBoolValue(leftVal.boolValue or rightVal.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  else: raise newException(RuntimeError, "Unknown binary operator")

method visitUnaryExpression*(visitor: InterpreterVisitor, node: UnaryExpression): Value {.base.} =
  let value = visitor.visitExpression(node.operand)

  case node.token.kind:
  of tkPlus: 
    case node.returnType.kind:
    of typeInt64, typeUint64: return value
    else: raise newException(RuntimeError, "Unsupported type for unary +")
  of tkMinus:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(-value.int64Value)
    else: raise newException(RuntimeError, "Unsupported type for unary -")
  of tkNot:
    case node.returnType.kind:
    of typeBool: return newBoolValue(not value.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for unary !")
  else: raise newException(RuntimeError, "Unknown binary operator")

method visitIdentifierExpression*(visitor: InterpreterVisitor, node: IdentifierExpression): Value {.base.} =
  return visitor.getValue(node.token.lexeme)

method visitCastExpression*(visitor: InterpreterVisitor, node: CastExpression): Value {.base.} =
  let value = visitor.visitExpression(node.value)
  let exception = newException(RuntimeError, "Cannot cast " & $value.valueType & " to " & $node.returnType)

  case value.valueTypeKind:
  of typeInt64:
    case node.returnType.kind:
    of typeInt64: return value
    of typeUint64: return newUint64Value(cast[uint](value.int64Value))
    of typeBool: return newBoolValue(value.int64Value != 0)
    of typeChar: return newCharValue(chr(value.int64Value))
    else: raise exception
  of typeUint64:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(cast[int](value.uint64Value))
    of typeUint64: return value
    of typeBool: return newBoolValue(value.uint64Value != 0)
    of typeChar: return newCharValue(chr(cast[int](value.uint64Value)))
    else: raise exception
  of typeBool:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(if value.boolValue: 1 else: 0)
    of typeUint64: return newUint64Value(if value.boolValue: 1 else: 0)
    of typeBool: return value
    of typeChar: return newCharValue(if value.boolValue: '\1' else: '\0')
    else: raise exception
  of typePtr:
    case node.returnType.kind:
    of typeBool: return newBoolValue(value.ptrValue != nil)
    else: raise exception
  of typeChar:
    case node.returnType.kind:
    of typeInt64: return newInt64Value(ord(value.charValue))
    of typeUint64: return newUint64Value(cast[uint](ord(value.charValue)))
    of typeBool: return newBoolValue(value.charValue != '\0')
    of typeChar: return value
    else: raise exception
  else: raise exception

method visitDerefExpression*(visitor: InterpreterVisitor, node: DerefExpression): Value {.base.} =
  let ptrValue = visitor.visitExpression(node.operand)
  if ptrValue.valueTypeKind == typeNul:
    raise newException(RuntimeError, "Cannot dereference nul pointer")
  if ptrValue.valueTypeKind != typePtr:
    raise newException(RuntimeError, "Cannot dereference non-pointer")
  return ptrValue.ptrValue[]

method visitCharExpression*(visitor: InterpreterVisitor, node: CharExpression): Value {.base.} =
  let ch = node.token.lexeme
  if ch.len == 1:
    return newCharValue(ch[0])
  else:
    raise newException(RuntimeError, "Invalid character literal")

method visitArrayExpression*(visitor: InterpreterVisitor, node: ArrayExpression): Value {.base.} =
  var elements: seq[Value]
  for val in node.values:
    elements.add(visitor.visitExpression(val))
  
  let baseType = if elements.len > 0: elements[0].valueType else: getUndefinedType()
  return newArrayValue(elements, baseType)

method visitIndexExpression*(visitor: InterpreterVisitor, node: IndexExpression): Value {.base.} =
  let arr = visitor.visitExpression(node.operand)
  if arr.valueTypeKind != typeArray:
    raise newException(RuntimeError, "Cannot index non-array")
  
  let idx = visitor.visitExpression(node.index)
  if not isNumber(idx.valueType):
    raise newException(RuntimeError, "Index must be number")

  let len = int(arr.arrayValue.length.uint64Value)

  if idx.intValue >= len or (idx.intValue) < -len:
    raise newException(RuntimeError, "Index out of bounds")
  
  let index = ((idx.intValue mod len) + len) mod len
  
  return arr.arrayValue.elements[index]

method visitNulExpression*(visitor: InterpreterVisitor, node: NulExpression): Value {.base.} =
  return newNulValue()

# STATEMENTS

method visitDeclarationStatement*(visitor: InterpreterVisitor, node: DeclarationStatement): auto =
  visitor.setValue(node.name.lexeme, visitor.visitExpression(node.value))

method visitBlockStatement*(visitor: InterpreterVisitor, node: BlockStatement): auto =
  for stmt in node.statements:
    visitor.visitStatement(stmt)

method visitAssignmentStatement*(visitor: InterpreterVisitor, node: AssignmentStatement): auto =
  if node.left of IdentifierExpression:
    let name = IdentifierExpression(node.left).token
    visitor.setValue(name.lexeme, visitor.visitExpression(node.value))
  
  elif node.left of DerefExpression:
    let ptrValue = visitor.visitExpression(DerefExpression(node.left).operand)
    if ptrValue.valueTypeKind == typeNul:
      raise newException(RuntimeError, "Cannot dereference nul pointer")
    if ptrValue.valueTypeKind != typePtr:
      raise newException(RuntimeError, "Cannot dereference non-pointer")
    ptrValue.ptrValue[] = visitor.visitExpression(node.value)

  elif node.left of IndexExpression:
    let indexExpr = IndexExpression(node.left)
    let arr = visitor.visitExpression(indexExpr.operand)
    if arr.valueTypeKind != typeArray:
      raise newException(RuntimeError, "Cannot index non-array")
    
    let idx = visitor.visitExpression(indexExpr.index)
    if not isNumber(idx.valueType):
      raise newException(RuntimeError, "Index must be number")

    let len = int(arr.arrayValue.length.uint64Value)
    
    if idx.intValue >= len or (idx.intValue) < -len:
      raise newException(RuntimeError, "Index out of bounds")
    
    let index = idx.intValue
    let actualIdx = ((index mod len) + len) mod len
    
    arr.arrayValue.elements[actualIdx] = visitor.visitExpression(node.value)

method visitBranchingStatement*(visitor: InterpreterVisitor, node: BranchingStatement): auto =
  if visitor.visitExpression(node.condition).boolValue:
    visitor.pushScope()
    visitor.visitStatement(node.ifBlock)
    visitor.popScope()
    return
  
  for el in node.elifBlocks:
    let elifCondition = visitor.visitExpression(el.cond)
    if elifCondition.boolValue:
      visitor.pushScope()
      visitor.visitStatement(el.elifBlock)
      visitor.popScope()
      return
  
  if node.elseBlock != nil:
    visitor.pushScope()
    visitor.visitStatement(node.elseBlock)
    visitor.popScope()

method visitBreakStatement*(visitor: InterpreterVisitor, node: BreakStatement): auto =
  raise newException(BreakException, "")

method visitContinueStatement*(visitor: InterpreterVisitor, node: ContinueStatement): auto =
  raise newException(ContinueException, "")

method visitWhileStatement*(visitor: InterpreterVisitor, node: WhileStatement): auto =
  visitor.pushScope()
  while visitor.visitExpression(node.condition).boolValue:
    try:
      visitor.visitStatement(node.whileBlock)
    except BreakException: 
      break
    except ContinueException: 
      continue
  visitor.popScope()

# SPECIALS

proc get(self: SpecialExpression | SpecialStatement, key: string): Expression =
  for token, expr in self.namedArgs.pairs:
    let k = if token.kind == tkIntLiteral: token.lexeme else: token.lexeme
    if k == key:
      return expr
  return newErrorExpression(self.token)

proc has(self: SpecialExpression | SpecialStatement, key: string): bool =
  for token, _ in self.namedArgs.pairs:
    let k = token.lexeme
    if k == key:
      return true
  return false

proc format(values: varargs[Value], sep="", repr=false, escape=false): string =
  result = ""
  for val in values:
    if isInt(val.valueType): 
      result &= $val.intValue
    elif isUint(val.valueType): 
      result &= $val.uintValue
    elif val.valueType == getBoolType():
      result &= $val.boolValue
    elif val.valueType == getCharType():
      var c = $val.charValue
      if escape:
        c = strutils.escape(c)
      if repr:
        c = "'" & c & "'"
      result &= c
    elif val.valueType == getArrayType(getCharType()):
      var s = ""
      for ch in val.arrayValue.elements:
        s &= $ch.charValue
      if escape:
        s = strutils.escape(s)
      if repr:
        s = s.repr
      result &= s
    elif val.valueType == getNulType():
      result &= "nul"
    else:
      raise newException(RuntimeError, "unknown type to format")
    result &= sep

method visitSpecialExpression*(visitor: InterpreterVisitor, node: SpecialExpression): Value {.base.} =
  case node.kind:
  of skNew:
    let val = visitor.visitExpression(node.get("0"))
    let ptrValue = new(Value)
    ptrValue[] = val
    return newPtrValue(ptrValue, val.valueType)

  of skArr:
    let sizeExpr = node.get("1")
    let sizeVal = visitor.visitExpression(sizeExpr)
    let size = int(uintValue(sizeVal))
    
    let baseType = node.get("0").returnType
    var elements: seq[Value]

    if node.has("@"):
      for i in 0..<size:
        elements.add(Value(valueTypeKind: baseType.kind, valueType: baseType))
    else:
      let initExpr = node.get("0")
      for i in 0..<size:
        elements.add(visitor.visitExpression(initExpr))
    return newArrayValue(elements, baseType)

  of skLen:
    let arr = visitor.visitExpression(node.get("0"))
    if arr.valueTypeKind != typeArray:
      raise newException(RuntimeError, "len expects array")
    return newInt64Value(int64(arr.arrayValue.elements.len))

  of skFmt:
    var sep = ""
    if node.has("sep"):
      let sepVal = visitor.visitExpression(node.get("sep"))
      for ch in sepVal.arrayValue.elements:
        sep.add(ch.charValue)
    
    var repr = false
    if node.has("repr"):
      repr = visitor.visitExpression(node.get("repr")).boolValue
    
    var escape = false
    if node.has("escape"):
      escape = visitor.visitExpression(node.get("escape")).boolValue
    
    var values: seq[Value]
    for token, expr in node.namedArgs.pairs:
      if token.kind == tkIntLiteral:
        values.add(visitor.visitExpression(expr))
    
    var chars: seq[Value]
    for ch in format(values, sep, repr, escape):
      chars.add(newCharValue(ch))
    return newArrayValue(chars, getCharType())

  else:
    echo "[InterpreterVisitor] WARNING: unhandled special expression"

method visitSpecialStatement*(visitor: InterpreterVisitor, node: SpecialStatement): auto =
  case node.kind:
  of skPrint:
    var term = "\n"
    if node.has("term"):
      let termVal = visitor.visitExpression(node.get("term"))
      var s = ""
      for ch in termVal.arrayValue.elements:
        s.add(ch.charValue)
      term = s
    
    var values: seq[Value] = @[]
    for token, expr in node.namedArgs.pairs:
      if token.kind == tkIntLiteral:
        let val = visitor.visitExpression(expr)
        values.add(val)
    stdout.write(format(values) & term)

  of skFree:
    let val = visitor.visitExpression(node.get("0"))
    case val.valueTypeKind:
    of typePtr:
      val.ptrValue[] = Value(valueTypeKind: typeNul, valueType: getNulType())
    of typeArray:
      val.arrayValue.elements = @[]
      val.arrayValue.length = newUint64Value(0)
    else: discard

  of skAssert:
    let cond = visitor.visitExpression(node.get("0"))
    if not cond.boolValue:
      let msg = if node.has("1"):
        let msgVal = visitor.visitExpression(node.get("1"))
        var s = ""
        for ch in msgVal.arrayValue.elements:
          s.add(ch.charValue)
        s
      else:
        "assertion failed"
      raise newException(RuntimeError, msg & " [AssertionError]")

  else:
    echo "[InterpreterVisitor] WARNING: unhandled special statement"

# GENERAL

method visitExpression*(visitor: InterpreterVisitor, node: Expression): Value =
  if node of ErrorExpression: discard
  elif node of TypeExpression: return Value(valueType: getUndefinedType(), valueTypeKind: typeUndefined)
  elif node of NumberExpression:
    return visitor.visitNumberExpression(NumberExpression(node))
  elif node of BoolExpression:
    return visitor.visitBoolExpression(BoolExpression(node))
  elif node of BinaryExpression:
    return visitor.visitBinaryExpression(BinaryExpression(node))
  elif node of UnaryExpression:
    return visitor.visitUnaryExpression(UnaryExpression(node))
  elif node of IdentifierExpression:
    return visitor.visitIdentifierExpression(IdentifierExpression(node))
  elif node of CastExpression:
    return visitor.visitCastExpression(CastExpression(node))
  elif node of DerefExpression:
    return visitor.visitDerefExpression(DerefExpression(node))
  elif node of CharExpression:
    return visitor.visitCharExpression(CharExpression(node))
  elif node of ArrayExpression:
    return visitor.visitArrayExpression(ArrayExpression(node))
  elif node of IndexExpression:
    return visitor.visitIndexExpression(IndexExpression(node))
  elif node of NulExpression:
    return visitor.visitNulExpression(NulExpression(node))
  elif node of SpecialExpression:
    return visitor.visitSpecialExpression(SpecialExpression(node))
  else:
    echo "[InterpreterVisitor] WARNING: unhandled expression"

method visitStatement*(visitor: InterpreterVisitor, node: Statement) =
  if node of ErrorStatement: discard
  elif node of DeclarationStatement:
    visitor.visitDeclarationStatement(DeclarationStatement(node))
  elif node of BlockStatement:
    visitor.visitBlockStatement(BlockStatement(node))
  elif node of AssignmentStatement:
    visitor.visitAssignmentStatement(AssignmentStatement(node))
  elif node of BranchingStatement:
    visitor.visitBranchingStatement(BranchingStatement(node))
  elif node of SpecialStatement:
    visitor.visitSpecialStatement(SpecialStatement(node))
  elif node of BreakStatement:
    visitor.visitBreakStatement(BreakStatement(node))
  elif node of ContinueStatement:
    visitor.visitContinueStatement(ContinueStatement(node))
  elif node of WhileStatement:
    visitor.visitWhileStatement(WhileStatement(node))
  else:
    echo "[InterpreterVisitor] WARNING: unhandled statement"