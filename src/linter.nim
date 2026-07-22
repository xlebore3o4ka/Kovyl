import std/[os, json]
import core/[parser, astnodes, errors]
import core/visitors/[SemanticAnalyzerVisitor]
import utils/[strerr]

proc main() =
  let args = commandLineParams()
  
  if args.len == 0:
    stderr.writeLine("Usage: ", getAppFilename(), " <file>")
    quit(1)

  let filePath = args[0]
  let stdPath = getCurrentDir() / "src/std"
  
  if not fileExists(filePath):
    stderr.writeLine("Error: File not found: ", filePath)
    quit(1)
  
  let text = readFile(filePath)

  var parser = newParser(text, filePath)
  var blockStatement: BlockStatement = parser.parse()

  if errors.errors.len == 0:
    semanticAnalyzerLogging(false)
    try:
      newSemanticAnalyzerVisitor(stdPath).visitStatement(blockStatement)
    except ModuleError:
      discard

  stdout.write($toJson(filePath))

when isMainModule:
  main()