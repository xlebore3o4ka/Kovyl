type
  TypeKind* = enum
    typeUndefined

    typeInt64

    typeBool

  Type* = ref object
    case kind*: TypeKind
    else: discard

let
  undefinedType* = Type(kind: typeUndefined)
  int64Type* = Type(kind: typeInt64)
  boolType* = Type(kind: typeBool)

proc eq*(a: Type, b: Type): bool =
  if a == nil or b == nil: return false
  a.kind == b.kind

proc eq*(a: Type, b: TypeKind): bool =
  if a == nil: return false
  a.kind == b

proc eq*(a: TypeKind, b: Type): bool =
  if b == nil: return false
  a == b.kind

proc eq*(a: TypeKind, b: TypeKind): bool =
  a == b

proc neq*(a: Type | TypeKind, b: Type | TypeKind): bool =
  not eq(a, b)

proc getUndefinedType*(): Type {.inline.} = undefinedType
proc getInt64Type*():     Type {.inline.} = int64Type
proc getBoolType*():      Type {.inline.} = boolType

proc `$`*(k: TypeKind): string =
  case k
  of typeUndefined: "undefined"
  of typeInt64:     "int64"

  of typeBool:      "bool"

proc `$`*(t: Type): string =
  if t == nil: return "nilType"
  case t.kind
  else: return $t.kind