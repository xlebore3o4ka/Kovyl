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
  elif node of ErrorExpression:
    return "error " & $ErrorExpression(node).token
  return "Expr???"

proc `$`*(node: Statement): string =
  if node of VariableDeclarationStatement:
    let varStmt = VariableDeclarationStatement(node)
    return varStmt.typeToken.lexeme & " " & varStmt.name.lexeme & " = " & $varStmt.value
  elif node of BlockStatement:
    let blockStatement = BlockStatement(node)
    result = "do\n"
    for s in blockStatement.statements:
      result &= "  " & $s & "\n"
    result &= "end"
    return result
  elif node of ErrorStatement:
    return "error " & $ErrorStatement(node).token
  return "Stmt???"

proc representation*(node: Expression): string =
  if node of IntLitExpression:
    let intLit = IntLitExpression(node)
    return "IntLitExpression(" & intLit.value.lexeme & ")"
  
  elif node of BinaryExpression:
    let bin = BinaryExpression(node)
    return "BinaryExpression(" & representation(bin.left) & ", " & bin.op.lexeme & ", " & representation(bin.right) & ")"
  
  elif node of UnaryExpression:
    let unary = UnaryExpression(node)
    return "UnaryExpression(" & representation(unary.operand) & ", " & unary.op.lexeme & ")"
  
  elif node of ErrorExpression:
    let err = ErrorExpression(node)
    return "ErrorExpression(" & err.token.lexeme & ")"
  
  else:
    return "UnknownExpression()"

proc representation*(node: Statement): string =
  if node of VariableDeclarationStatement:
    let varStmt = VariableDeclarationStatement(node)
    return "VariableDeclarationStatement(" & varStmt.typeToken.lexeme & ", " & 
           varStmt.name.lexeme & ", " & representation(varStmt.value) & ")"
  
  elif node of BlockStatement:
    let blockStmt = BlockStatement(node)
    result = "BlockStatement(["
    for i, stmt in blockStmt.statements:
      if i > 0: result &= ", "
      result &= representation(stmt)
    result &= "])"
    return result
  
  elif node of ErrorStatement:
    let errStmt = ErrorStatement(node)
    return "ErrorStatement(" & errStmt.token.lexeme & ")"
  
  else:
    return "UnknownStatement()"