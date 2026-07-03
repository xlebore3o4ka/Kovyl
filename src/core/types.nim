type
  TypeKind* = enum
    typeUndefined
    typeInt
    typeUint
    typeBool
    typePtr

  Type* = object
    case kind*: TypeKind
    of typePtr:
      ptrBaseType*: ptr Type
    else: discard

let
  undefinedType = Type(kind: typeUndefined)
  intType = Type(kind: typeInt)
  uintType = Type(kind: typeUint)
  boolType = Type(kind: typeBool)
var 
  ptrTypes: seq[Type] = @[]

proc getUndefinedType*(): ptr Type = addr undefinedType
proc getIntType*(): ptr Type = addr intType
proc getUintType*(): ptr Type = addr uintType
proc getBoolType*(): ptr Type = addr boolType

proc getPtrType*(baseType: ptr Type): ptr Type =
  for t in ptrTypes:
    if t.kind == typePtr and t.ptrBaseType == baseType:
      return addr t
  
  ptrTypes.add(Type(kind: typePtr, ptrBaseType: baseType))
  return addr ptrTypes[^1]

proc `$`*(t: ptr Type): string {.inline.}

proc `$`*(t: Type): string =
  case t.kind
  of typeUndefined: "undefined"
  of typeInt: "int"
  of typeUint: "uint"
  of typeBool: "bool"
  of typePtr: "ptr[" & $t.ptrBaseType & "]"

proc `$`*(t: ptr Type): string {.inline.} =
  if t == nil: return "undefined.nil"
  return $t[]