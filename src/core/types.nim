type
  TypeKind* = enum
    typeUndefined
    typeInt
    typeUint
    typeBool
    typePtr
    typeChar

  Type* = ref object
    case kind*: TypeKind
    of typePtr: ptrBaseType*: Type
    else: discard

let
  undefinedType* = Type(kind: typeUndefined)
  intType* = Type(kind: typeInt)
  uintType* = Type(kind: typeUint)
  boolType* = Type(kind: typeBool)
  charType* = Type(kind: typeChar)

var ptrTypes*: seq[Type] = @[]

proc getUndefinedType*(): Type {.inline.} = undefinedType
proc getIntType*(): Type {.inline.} = intType
proc getUintType*(): Type {.inline.} = uintType
proc getBoolType*(): Type {.inline.} = boolType
proc getCharType*(): Type {.inline.} = charType

proc getPtrType*(baseType: Type): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in ptrTypes:
    if t.kind == typePtr and t.ptrBaseType == baseType:
      return t
  
  result = Type(kind: typePtr, ptrBaseType: baseType)
  ptrTypes.add(result)

proc `$`*(t: Type): string =
  if t == nil: return "nil"
  case t.kind
  of typeUndefined: "undefined"
  of typeInt: "int"
  of typeUint: "uint"
  of typeBool: "bool"
  of typePtr: $t.ptrBaseType & "*"
  of typeChar: "char"