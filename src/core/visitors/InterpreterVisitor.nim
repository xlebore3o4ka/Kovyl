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
    of typeInt: intValue*: int
    of typeUint: uintValue*: uint
    of typeBool: boolValue*: bool
    of typePtr, typeNul: ptrValue*: ref Value
    of typeChar: charValue*: char
    of typeArray: arrayValue*: ArrayValue

  InterpreterVisitor* = ref object of Visitor
    literalTable*: Table[string, Value] = initTable[string, Value]()

proc `==`*(a, b: Value): bool =
  if a.valueType != b.valueType:
    return false
  if a.valueTypeKind != b.valueTypeKind:
    return false
  
  case a.valueTypeKind
  of typeInt: return a.intValue == b.intValue
  of typeUint: return a.uintValue == b.uintValue
  of typeBool: return a.boolValue == b.boolValue
  of typeChar: return a.charValue == b.charValue
  of typePtr: return a.ptrValue == b.ptrValue
  of typeArray: return a.arrayValue == b.arrayValue
  else: 
    raise newException(RuntimeError, "The type '" & $a.valueType & "' cannot be compared")

proc `==`*(a, b: ArrayValue): bool =
  if a.elements.len != b.elements.len:
    return false
  for i in 0..<a.elements.len:
    if a.elements[i] != b.elements[i]:
      return false
  return true

proc newIntValue*(intValue: int): Value {.inline.} =
  Value(valueTypeKind: typeInt, valueType: getIntType(), intValue: intValue)

proc newUintValue*(uintValue: uint): Value {.inline.} =
  Value(valueTypeKind: typeUint, valueType: getUintType(), uintValue: uintValue)

proc newBoolValue*(boolValue: bool): Value {.inline.} =
  Value(valueTypeKind: typeBool, valueType: getBoolType(), boolValue: boolValue)

proc newPtrValue*(ptrVal: ref Value, baseType: Type): Value =
  Value(valueTypeKind: typePtr, valueType: getPtrType(baseType), ptrValue: ptrVal)

proc newCharValue*(ch: char): Value {.inline.} =
  Value(valueTypeKind: typeChar, valueType: getCharType(), charValue: ch)

proc newArrayValue*(elements: seq[Value], baseType: Type): Value =
  Value(valueTypeKind: typeArray, valueType: getArrayType(baseType), arrayValue: 
    ArrayValue(elements: elements, length: newUintValue(uint(elements.len))))

proc newNulValue(): Value =
  Value(valueTypeKind: typeNul, valueType: getNulType())

proc newInterpreterVisitor*(): InterpreterVisitor {.inline.} =
  InterpreterVisitor()

method visitExpression*(visitor: InterpreterVisitor, node: Expression): Value {.base.}
method visitStatement*(visitor: InterpreterVisitor, node: Statement) {.base.}

method visitIntExpression*(visitor: InterpreterVisitor, node: IntExpression): Value {.base.} =
  return newIntValue(parseInt(node.token.lexeme))

method visitBoolExpression*(visitor: InterpreterVisitor, node: BoolExpression): Value {.base.} =
  return newBoolValue(node.token.kind == tkTrue)

method visitBinaryExpression*(visitor: InterpreterVisitor, node: BinaryExpression): Value {.base.} =
  let leftVal = visitor.visitExpression(node.left)
  let rightVal = visitor.visitExpression(node.right)

  case node.token.kind:
  of tkPlus:
    case node.returnType.kind:
    of typeInt: return newIntValue(leftVal.intValue + rightVal.intValue)
    of typeUint: return newUintValue(leftVal.uintValue + rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkMinus:
    case node.returnType.kind:
    of typeInt: return newIntValue(leftVal.intValue - rightVal.intValue)
    of typeUint: return newUintValue(leftVal.uintValue - rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkStar:
    case node.returnType.kind:
    of typeInt: return newIntValue(leftVal.intValue * rightVal.intValue)
    of typeUint: return newUintValue(leftVal.uintValue * rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkSlash:
    case node.returnType.kind:
    of typeInt: return newIntValue(leftVal.intValue div rightVal.intValue)
    of typeUint: return newUintValue(leftVal.uintValue div rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkEQ:
    return newBoolValue(leftVal == rightVal)
  of tkNEQ:
    return newBoolValue(leftVal != rightVal)
  of tkGT:
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue > rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue > rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkLT:
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue < rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue < rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkGTE:
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue >= rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue >= rightVal.uintValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkLTE:
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue <= rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue <= rightVal.uintValue)
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
    of typeInt, typeUint: return value
    else: raise newException(RuntimeError, "Unsupported type for unary +")
  of tkMinus:
    case node.returnType.kind:
    of typeInt: return newIntValue(-value.intValue)
    else: raise newException(RuntimeError, "Unsupported type for unary -")
  of tkNot:
    case node.returnType.kind:
    of typeBool: return newBoolValue(not value.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for unary !")
  else: raise newException(RuntimeError, "Unknown binary operator")

method visitIdentifierExpression*(visitor: InterpreterVisitor, node: IdentifierExpression): Value {.base.} =
  return visitor.literalTable[node.token.lexeme]

method visitCastExpression*(visitor: InterpreterVisitor, node: CastExpression): Value {.base.} =
  let value = visitor.visitExpression(node.value)
  let exception = newException(RuntimeError, "Cannot cast " & $value.valueType & " to " & $node.returnType)

  case value.valueTypeKind:
  of typeInt:
    case node.returnType.kind:
    of typeInt: return value
    of typeUint: return newUintValue(cast[uint](value.intValue))
    of typeBool: return newBoolValue(value.intValue != 0)
    of typeChar: return newCharValue(chr(value.intValue))
    else: raise exception
  of typeUint:
    case node.returnType.kind:
    of typeInt: return newIntValue(cast[int](value.uintValue))
    of typeUint: return value
    of typeBool: return newBoolValue(value.uintValue != 0)
    of typeChar: return newCharValue(chr(cast[int](value.uintValue)))
    else: raise exception
  of typeBool:
    case node.returnType.kind:
    of typeInt: return newIntValue(if value.boolValue: 1 else: 0)
    of typeUint: return newUintValue(if value.boolValue: 1 else: 0)
    of typeBool: return value
    of typeChar: return newCharValue(if value.boolValue: '\1' else: '\0')
    else: raise exception
  of typePtr:
    case node.returnType.kind:
    of typeBool: return newBoolValue(value.ptrValue != nil)
    else: raise exception
  of typeChar:
    case node.returnType.kind:
    of typeInt: return newIntValue(ord(value.charValue))
    of typeUint: return newUintValue(cast[uint](ord(value.charValue)))
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
  if idx.valueTypeKind != typeInt:
    raise newException(RuntimeError, "Index must be int")

  let len = int(arr.arrayValue.length.uintValue)

  if idx.intValue >= len or (idx.intValue) < -len:
    raise newException(RuntimeError, "Index out of bounds")
  
  let index = ((idx.intValue mod len) + len) mod len
  
  return arr.arrayValue.elements[index]

method visitNulExpression*(visitor: InterpreterVisitor, node: NulExpression): Value {.base.} =
  return newNulValue()

# STATEMENTS

method visitDeclarationStatement*(visitor: InterpreterVisitor, node: DeclarationStatement): auto =
  visitor.literalTable[node.name.lexeme] = visitor.visitExpression(node.value)

method visitBlockStatement*(visitor: InterpreterVisitor, node: BlockStatement): auto =
  for stmt in node.statements:
    visitor.visitStatement(stmt)

method visitAssignmentStatement*(visitor: InterpreterVisitor, node: AssignmentStatement): auto =
  if node.left of IdentifierExpression:
    let name = IdentifierExpression(node.left).token
    visitor.literalTable[name.lexeme] = visitor.visitExpression(node.value)
  
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
    if idx.valueTypeKind != typeInt:
      raise newException(RuntimeError, "Index must be int")

    let len = int(arr.arrayValue.length.uintValue)
    
    if idx.intValue >= len or (idx.intValue) < -len:
      raise newException(RuntimeError, "Index out of bounds")
    
    let index = idx.intValue
    let actualIdx = ((index mod len) + len) mod len
    
    arr.arrayValue.elements[actualIdx] = visitor.visitExpression(node.value)

method visitBranchingStatement*(visitor: InterpreterVisitor, node: BranchingStatement): auto =
  let conditionValue = visitor.visitExpression(node.condition)
  
  if conditionValue.boolValue:
    visitor.visitStatement(node.ifBlock)
    return
  
  for el in node.elifBlocks:
    let elifCondition = visitor.visitExpression(el.cond)
    if elifCondition.boolValue:
      visitor.visitStatement(el.elifBlock)
      return
  
  if node.elseBlock != nil:
    visitor.visitStatement(node.elseBlock)

# SPECIALS

method visitSpecialExpression*(visitor: InterpreterVisitor, node: SpecialExpression): Value {.base.} =
  case node.kind:
  of skNew: 
    let val = visitor.visitExpression(node.args[0])
    let ptrValue = new(Value)
    ptrValue[] = val
    return newPtrValue(ptrValue, val.valueType)

  of skArr:     
    let sizeVal = visitor.visitExpression(node.args[1])

    let size = int(sizeVal.uintValue)
    var elements: seq[Value]

    for i in 0..<size:
      let baseType = node.args[0].returnType
      elements.add(Value(valueTypeKind: baseType.kind, valueType: baseType))

    return newArrayValue(elements, node.args[0].returnType)

  of skLen: 
    let arr = visitor.visitExpression(node.args[0])
    return arr.arrayValue.length

  else: 
    echo "[InterpreterVisitor] WARNING: unhandled special expression"

method visitSpecialStatement*(visitor: InterpreterVisitor, node: SpecialStatement): auto =
  case node.kind:
  of skOut:
    for arg in node.args:
      let val = visitor.visitExpression(arg)
      case val.valueTypeKind:
      of typeInt: stdout.write($val.intValue)
      of typeUint: stdout.write($val.uintValue)
      of typeBool: stdout.write($val.boolValue)
      of typeChar: stdout.write(val.charValue)
      of typeArray:
        if val.valueType.arrayBaseType == getCharType():
          var s = ""
          for ch in val.arrayValue.elements:
            s.add(ch.charValue)
          stdout.write(s)
        else:
          raise newException(RuntimeError, "Cannot output " & $val.valueType)
      else: raise newException(RuntimeError, "Cannot output " & $val.valueType)
    stdout.write('\n')
  
  of skFree:
    let val = visitor.visitExpression(node.args[0])
    
    case val.valueTypeKind:
    of typePtr:
      if val.ptrValue != nil:
        val.ptrValue[] = Value(valueTypeKind: typeUndefined, valueType: getUndefinedType())

    of typeArray:
      if val.arrayValue.length.uintValue != 0u:
        val.arrayValue.elements = @[]
        val.arrayValue.length = newUintValue(0)
    else:
      raise newException(RuntimeError, "free expects pointer or array")

  else: 
    echo "[InterpreterVisitor] WARNING: unhandled special expression"

# GENERAL

method visitExpression*(visitor: InterpreterVisitor, node: Expression): Value =
  if node of ErrorExpression: discard
  elif node of TypeExpression: return Value(valueType: getUndefinedType(), valueTypeKind: typeUndefined)
  elif node of IntExpression:
    return visitor.visitIntExpression(IntExpression(node))
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
  else:
    echo "[InterpreterVisitor] WARNING: unhandled statement"