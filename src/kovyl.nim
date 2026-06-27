import core/[parser, astnodes, errors]
import utils/[strerr]
import os
import core/visitors/ASTPrinterVisitor

proc main() =
  let args = commandLineParams()
  
  if args.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " <file>")
    quit(1)
  
  var shortErrors = false
  var filePath = ""
  
  for arg in args:
    if arg == "-s":
      shortErrors = true
    else:
      filePath = arg
  
  if filePath.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " [-t] [-r] <file>")
    quit(1)
  
  if not fileExists(filePath):
    stderr.writeLine("Error: File not found: ", filePath)
    quit(1)
  
  let text = readFile(filePath)

  stdout.writeLine("")

  var parser = newParser(text, filePath)
  var blockStatement: BlockStatement = parser.parse()
  
  if errors.errors.len == 0:
    echo newASTPrinterVisitor().printStatement(blockStatement)
  else:
    for error in errors.errors:
      printError(error, shortErrors)

when isMainModule:
  main()