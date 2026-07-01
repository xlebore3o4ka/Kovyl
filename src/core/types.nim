type
  TypeKind* = enum
    typeUndefined
    typeInt
    typeUint
    typeBool
    typeString

  Type* = object
    case kind*: TypeKind
    else: discard

let
  undefinedType = Type(kind: typeUndefined)
  intType = Type(kind: typeInt)
  uintType = Type(kind: typeUint)
  boolType = Type(kind: typeBool)
  stringType = Type(kind: typeString)

proc getUndefinedType*(): ptr Type = addr undefinedType
proc getIntType*(): ptr Type = addr intType
proc getUintType*(): ptr Type = addr uintType
proc getBoolType*(): ptr Type = addr boolType
proc getStringType*(): ptr Type = addr stringType

proc `$`*(t: Type): string =
  case t.kind
  of typeUndefined: "undefined"
  of typeInt: "int"
  of typeUint: "uint"
  of typeBool: "bool"
  of typeString: "string"

proc `$`*(t: ptr Type): string {.inline.} =
  if t == nil: return "nil"
  return $t[]