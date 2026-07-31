import os
import core/[errors, parser]
import visitors/[semantics, codegen]

proc main() =
  if paramCount() < 1:
    echo "Using: ", getAppFilename().extractFilename(), " <file>"
    return

  let filename = paramStr(1)
  if not fileExists(filename):
    echo "Error: file not found - ", filename
    return

  let content = readFile(filename)
  var parser = newParser(content, filename)

  let expression = parser.parse()

  block errorProne:
    if errors.errors.len != 0: break errorProne
    checkSemantics(expression)
    
    if errors.errors.len != 0: break errorProne
    echo generate(expression)

  if errors.errors.len != 0:
    for e in errors.errors:
      echo e.kind, ' ', e.message, ' ', e.args

when isMainModule:
  main()