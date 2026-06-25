type
  TypeKind = enum
    typeUndefined, typeInt

  Type* = object
    case kind*: TypeKind
    else: discard

let
  undefinedType = Type(kind: typeUndefined)
  intType = Type(kind: typeInt)

proc getUndefinedType*(): ptr Type = addr undefinedType
proc getIntType*(): ptr Type = addr intType