import std/strutils

type
  TypeKind* = enum
    typeUndefined
    typeInt64
    typeInt32
    typeInt16
    typeInt8
    typeUint64
    typeUint32
    typeUint16
    typeUint8
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
  of typeInt64: "int64"
  of typeInt32: "int32"
  of typeInt16: "int16"
  of typeInt8: "int8"
  of typeUint64: "uint64"
  of typeUint32: "uint32"
  of typeUint16: "uint16"
  of typeUint8: "uint8"
  of typeBool: "bool"
  of typePtr: $t.ptrBaseType & "*"
  of typeChar: "char"
  of typeArray: $t.arrayBaseType & "[]"
  of typeNul: "nul"

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
  of typePtr: "type*"
  of typeChar: "char"
  of typeArray: "type[]"
  of typeNul: "nul"

proc getPrimitiveType*(t: Type): Type =
  var current = t
  while true:
    case current.kind
    of typePtr:
      current = current.ptrBaseType
    of typeArray:
      current = current.arrayBaseType
    else:
      return current

proc isNumber*(t: Type): bool =
  t.kind in {typeInt64, typeInt32, typeInt16, typeInt8, typeUint64, typeUint32, typeUint16, typeUint8}

proc isValidInt8*(s: string): bool =
  try:
    let v = parseInt(s)
    return v >= -128 and v <= 127
  except ValueError:
    return false

proc isValidInt16*(s: string): bool =
  try:
    let v = parseInt(s)
    return v >= -32768 and v <= 32767
  except ValueError:
    return false

proc isValidInt32*(s: string): bool =
  try:
    let v = parseInt(s)
    return v >= -2147483648 and v <= 2147483647
  except ValueError:
    return false

proc isValidInt64*(s: string): bool =
  try:
    discard parseInt(s)
    return true
  except ValueError:
    return false

proc isValidUint8*(s: string): bool =
  try:
    let v = parseUInt(s)
    return v <= 255
  except ValueError:
    return false

proc isValidUint16*(s: string): bool =
  try:
    let v = parseUInt(s)
    return v <= 65535
  except ValueError:
    return false

proc isValidUint32*(s: string): bool =
  try:
    let v = parseUInt(s)
    return v <= 4294967295'u64
  except ValueError:
    return false

proc isValidUint64*(s: string): bool =
  try:
    discard parseUInt(s)
    return true
  except ValueError:
    return false