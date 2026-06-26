import core/[parser, astnodes, errors, lexer, tokens]
import utils/[strast, strerr, strtok]
import os

proc main() =
  let args = commandLineParams()
  
  if args.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " <file>")
    quit(1)
  
  var showTokens = false
  var showRepr = false
  var filePath = ""
  
  for arg in args:
    if arg == "-t":
      showTokens = true
    elif arg == "-r":
      showRepr = true
    else:
      filePath = arg
  
  if filePath.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " [-t] [-r] <file>")
    quit(1)
  
  if not fileExists(filePath):
    stderr.writeLine("Error: File not found: ", filePath)
    quit(1)
  
  let text = readFile(filePath)
  
  if showTokens:
    var lexer = newLexer(text, filePath)
    while true:
      let token = lexer.nextToken()
      if token.kind == tkEOS:
        stdout.writeLine("")
      else:
        stdout.write("(" & token.mean() & ")`" & token.lexeme & "` ")
        if token.kind == tkEOF: break

  stdout.writeLine("")

  var parser = newParser(text, filePath)
  var blockStatement: BlockStatement = parser.parse()
  
  if errors.errors.len == 0:
    if showRepr:
      echo blockStatement.representation
    else:
      echo blockStatement
  else:
    for error in errors.errors:
      printError(error)

when isMainModule:
  main()