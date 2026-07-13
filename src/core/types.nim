type
  TypeKind* = enum
    typeUndefined
    typeInt64, typeInt32, typeInt16, typeInt8
    typeUint64, typeUint32, typeUint16, typeUint8

    typeBool
    typeChar
    typeStaticArray

    typePtr
    typeArray
    typeNul

  Type* = ref object
    case kind*: TypeKind
    of typePtr: ptrBase*: Type
    of typeArray: arrBase*: Type
    of typeStaticArray:
      staticArrBase*: Type
      length*: Natural
    else: discard

let
  undefinedType* = Type(kind: typeUndefined)
  int64Type* = Type(kind: typeInt64)
  int32Type* = Type(kind: typeInt32)
  int16Type* = Type(kind: typeInt16)
  int8Type* = Type(kind: typeInt8)
  uint64Type* = Type(kind: typeUint64)
  uint32Type* = Type(kind: typeUint32)
  uint16Type* = Type(kind: typeUint16)
  uint8Type* = Type(kind: typeUint8)
  boolType* = Type(kind: typeBool)
  charType* = Type(kind: typeChar)

  nulType* = Type(kind: typeNul)

var ptrTypes*: seq[Type] = @[]
var arrayTypes*: seq[Type] = @[]
var staticArrayTypes*: seq[Type] = @[]

proc getUndefinedType*(): Type {.inline.} = undefinedType
proc getInt64Type*(): Type {.inline.} = int64Type
proc getInt32Type*(): Type {.inline.} = int32Type
proc getInt16Type*(): Type {.inline.} = int16Type
proc getInt8Type*(): Type {.inline.} = int8Type
proc getUint64Type*(): Type {.inline.} = uint64Type
proc getUint32Type*(): Type {.inline.} = uint32Type
proc getUint16Type*(): Type {.inline.} = uint16Type
proc getUint8Type*(): Type {.inline.} = uint8Type
proc getBoolType*(): Type {.inline.} = boolType
proc getCharType*(): Type {.inline.} = charType

proc getNulType*(): Type {.inline.} = nulType

proc eq*(a: TypeKind, b: TypeKind): bool {.inline.} =
  return a == b

proc eq*(a: TypeKind, b: Type): bool {.inline.} =
  return a == b.kind

proc eq*(a: Type, b: TypeKind): bool {.inline.} =
  return a.kind == b

proc eq*(a: Type, b: Type): bool =
  if a.eq(typeStaticArray) and a.length == 0:
    return b.eq(typeStaticArray)
  if b.eq(typeStaticArray) and b.length == 0:
    return a.eq(typeStaticArray)

  return a == b

proc neq*(a: Type | TypeKind, b: Type | TypeKind): bool {.inline.} =
  return not (a.eq b)

proc getPtrType*(baseType: Type): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in ptrTypes:
    if t.kind == typePtr and t.ptrBase.eq baseType:
      return t
  
  result = Type(kind: typePtr, ptrBase: baseType)
  ptrTypes.add(result)

proc getArrayType*(baseType: Type): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in arrayTypes:
    if t.kind == typeArray and t.arrBase.eq baseType:
      return t
  
  result = Type(kind: typeArray, arrBase: baseType)
  arrayTypes.add(result)

proc getStaticArrayType*(baseType: Type, length: Natural): Type =
  if baseType.kind == typeUndefined:
    return baseType

  for t in staticArrayTypes:
    if t.kind == typeStaticArray and t.staticArrBase.eq(baseType) and t.length == length:
      return t
  
  result = Type(kind: typeStaticArray, staticArrBase: baseType, length: length)
  staticArrayTypes.add(result)

proc `$`*(k: TypeKind): string =
  case k
  of typeUndefined: "undefined"
  of typeInt64: "int64"
  of typeInt32: "int32"
  of typeInt16: "int16"
  of typeInt8: "int8"
  of typeUint64: "uint64"
  of typeUint32: "uint32"
  of typeUint16: "uint16"
  of typeUint8: "uint8"
  of typeBool: "bool"
  of typePtr: "Any*"
  of typeChar: "char"
  of typeArray: "Any[*]"
  of typeStaticArray: "Any[]"
  of typeNul: "nul"

proc `$`*(t: Type): string =
  if t == nil: return "nilType"
  case t.kind
  of typePtr: $t.ptrBase & "*"
  of typeArray: $t.arrBase & "[*]"
  of typeStaticArray: $t.staticArrBase & "[" & (if t.length == 0: "" 
    else: $t.length) & "]" 
  else: return $t.kind