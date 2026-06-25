import ../core/astnodes

func `$`*(node: Expression): string =
  if node of IntLitExpression:
    return IntLitExpression(node).value.lexeme
  elif node of BinaryExpression:
    let bin = BinaryExpression(node)
    return "(" & $bin.left & " " & bin.op.lexeme & " " & $bin.right & ")"
  elif node of UnaryExpression:
    let unary = UnaryExpression(node)
    return "(" & unary.op.lexeme & $unary.operand & ")"
  return "???"