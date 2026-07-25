import core/[parser, astnodes, errors]
import utils/[strerr]
import std/[os, json]
import core/visitors/[SemanticAnalyzerVisitor, InterpreterVisitor]

proc main() =
  let args = commandLineParams()
  
  if args.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " <file>")
    quit(1)
  
  var shortErrors = false
  var showAST = false
  var debug = false
  var lintMode = false
  var filePath = ""

  let stdPath = getCurrentDir() / "src/std"
  
  for arg in args:
    if arg == "-s":
      shortErrors = true
    elif arg == "-a":
      showAST = true
    elif arg == "-d":
      debug = true
    elif arg == "-l":
      lintMode = true
    else:
      filePath = arg
  
  if filePath.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " [-s] [-a] [-d] [-l] <file>")
    quit(1)
  
  if not fileExists(filePath):
    stderr.writeLine("Error: File not found: ", filePath)
    quit(1)
  
  let text = readFile(filePath)

  if lintMode:
    stdout.writeLine("")
    
    var parser = newParser(text, filePath)
    var blockStatement: BlockStatement = parser.parse()
    
    if errors.errors.len == 0:
      semanticAnalyzerLogging(false)
      try:
        newSemanticAnalyzerVisitor(stdPath).visitStatement(blockStatement)
      except ModuleError:
        discard
    
    stdout.write($toJson(filePath))
    
    return

  stdout.writeLine("")

  var parser = newParser(text, filePath)
  var blockStatement: BlockStatement = parser.parse()

  semanticAnalyzerLogging(false)
  interpreterVisitorLogging(false)
  
  if errors.errors.len == 0:
    if debug:
      semanticAnalyzerLogging(true)
    try:
      newSemanticAnalyzerVisitor(stdPath).visitStatement(blockStatement)
    except ModuleError:
      discard
  
  if errors.errors.len == 0:
    if debug:
      echo "[KOVYL] INFO: Compilation successful!"
      semanticAnalyzerLogging(false)
      interpreterVisitorLogging(true)
      
    let interpreter = newInterpreterVisitor()
    interpreter.visitStatement(blockStatement)

    if debug:
      echo "\n[KOVYL] INFO: Running successful!"
    interpreterVisitorLogging(false)

  for error in errors.errors:
    printError(error, shortErrors)

when isMainModule:
  main()