type
  TypeKind* = enum
    typeUndefined
    typeInt
    typeUint
    typeBool
    typePtr
    typeChar
    typeArray

    typeNul

  Type* = ref object
    case kind*: TypeKind
    of typePtr: ptrBaseType*: Type
    of typeArray: arrayBaseType*: Type
    else: discard

let
  undefinedType* = Type(kind: typeUndefined)
  intType* = Type(kind: typeInt)
  uintType* = Type(kind: typeUint)
  boolType* = Type(kind: typeBool)
  charType* = Type(kind: typeChar)

  nulType* = Type(kind: typeNul)

var ptrTypes*: seq[Type] = @[]
var arrayTypes*: seq[Type] = @[]

proc getUndefinedType*(): Type {.inline.} = undefinedType
proc getIntType*(): Type {.inline.} = intType
proc getUintType*(): Type {.inline.} = uintType
proc getBoolType*(): Type {.inline.} = boolType
proc getCharType*(): Type {.inline.} = charType

proc getNulType*(): Type {.inline.} = nulType

proc getPtrType*(baseType: Type): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in ptrTypes:
    if t.kind == typePtr and t.ptrBaseType == baseType:
      return t
  
  result = Type(kind: typePtr, ptrBaseType: baseType)
  ptrTypes.add(result)

proc getArrayType*(baseType: Type): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in arrayTypes:
    if t.kind == typeArray and t.arrayBaseType == baseType:
      return t
  
  result = Type(kind: typeArray, arrayBaseType: baseType)
  arrayTypes.add(result)

proc `$`*(t: Type): string =
  if t == nil: return "nilType"
  case t.kind
  of typeUndefined: "undefined"
  of typeInt: "int"
  of typeUint: "uint"
  of typeBool: "bool"
  of typePtr: $t.ptrBaseType & "*"
  of typeChar: "char"
  of typeArray: $t.arrayBaseType & "[]"

  of typeNul: "nul"

proc `$`*(k: TypeKind): string =
  case k
  of typeUndefined: "undefined"
  of typeInt: "int"
  of typeUint: "uint"
  of typeBool: "bool"
  of typePtr: "ptr"
  of typeChar: "char"
  of typeArray: "array"
  of typeNul: "nul"