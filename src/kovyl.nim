import core/[parser, astnodes, errors]
import utils/[strast, strerr]
import os

proc main() =
  let args = commandLineParams()
  
  if args.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " <file>")
    quit(1)
  
  let filePath = args[0]
  
  if not fileExists(filePath):
    stderr.writeLine("Error: File not found: ", filePath)
    quit(1)
  
  let text = readFile(filePath)
  var parser = newParser(text, filePath)
  var expression: Expression = parser.parse()
  
  if errors.errors.len == 0:
    echo expression
  else:
    for error in errors.errors:
      printError(error)

when isMainModule:
  main()