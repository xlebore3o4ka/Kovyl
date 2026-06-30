import visitor
import ../[astnodes, types, tokens]
import std/[strutils, tables]

type
  RuntimeError* = object of CatchableError

  Value* = object
    valueType*: ptr Type
    case valueTypeKind*: TypeKind
    of typeUndefined: discard
    of typeInt: intValue*: int
    of typeUint: uintValue*: uint
    of typeBool: boolValue*: bool

  InterpreterVisitor* = ref object of Visitor
    literalTable*: Table[string, Value] = initTable[string, Value]()

proc newIntValue*(intValue: int): Value {.inline.} =
  Value(valueTypeKind: typeInt, valueType: getIntType(), intValue: intValue)

proc newUintValue*(uintValue: uint): Value {.inline.} =
  Value(valueTypeKind: typeUint, valueType: getUintType(), uintValue: uintValue)

proc newBoolValue*(boolValue: bool): Value {.inline.} =
  Value(valueTypeKind: typeBool, valueType: getBoolType(), boolValue: boolValue)

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
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue == rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue == rightVal.uintValue)
    of typeBool: return newBoolValue(leftVal.boolValue == rightVal.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
  of tkNEQ:
    case node.left.returnType.kind:
    of typeInt: return newBoolValue(leftVal.intValue != rightVal.intValue)
    of typeUint: return newBoolValue(leftVal.uintValue != rightVal.uintValue)
    of typeBool: return newBoolValue(leftVal.boolValue != rightVal.boolValue)
    else: raise newException(RuntimeError, "Unsupported type for binary " & node.token.mean())
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

  case value.valueTypeKind:
  of typeInt:
    case node.returnType.kind:
    of typeInt: return value
    of typeUint: return newUintValue(cast[uint](value.intValue))
    of typeBool: return newBoolValue(value.intValue != 0)
    else: raise newException(RuntimeError, "Unknown type to convert")
  of typeUint:
    case node.returnType.kind:
    of typeInt: return newIntValue(cast[int](value.uintValue))
    of typeUint: return value
    of typeBool: return newBoolValue(value.uintValue != 0)
    else: raise newException(RuntimeError, "Unknown type to convert")
  of typeBool:
    case node.returnType.kind:
    of typeInt: return newIntValue(if value.boolValue: 1 else: 0)
    of typeUint: return newUintValue(if value.boolValue: 1 else: 0)
    of typeBool: return value
    else: raise newException(RuntimeError, "Unknown type to convert")
  else: raise newException(RuntimeError, "Unknown type to convert")

method visitDeclarationStatement*(visitor: InterpreterVisitor, node: DeclarationStatement): auto =
  visitor.literalTable[node.name.lexeme] = visitor.visitExpression(node.value)

method visitBlockStatement*(visitor: InterpreterVisitor, node: BlockStatement): auto =
  for stmt in node.statements:
    visitor.visitStatement(stmt)

method visitAssignmentStatement*(visitor: InterpreterVisitor, node: AssignmentStatement): auto =
  visitor.literalTable[node.name.lexeme] = visitor.visitExpression(node.value)

method visitOutStatement*(visitor: InterpreterVisitor, node: OutStatement): auto =
  let value = visitor.visitExpression(node.value)
  case value.valueTypeKind:
  of typeInt: echo value.intValue
  of typeUint: echo value.uintValue
  of typeBool: echo value.boolValue
  else: echo ""

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

method visitExpression*(visitor: InterpreterVisitor, node: Expression): Value =
  if node of ErrorExpression: discard
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
  elif node of OutStatement:
    visitor.visitOutStatement(OutStatement(node))
  elif node of BranchingStatement:
    visitor.visitBranchingStatement(BranchingStatement(node))
  else:
    echo "[InterpreterVisitor] WARNING: unhandled statement"