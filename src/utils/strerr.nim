import ../core/errors
import std/[strutils, strformat, os, json]

proc getSourceLine(file: string, line: Natural): string =
  try:
    let content = readFile(file)
    let lines = content.splitLines()
    if line <= lines.len:
      return lines[line - 1].strip(leading = false, trailing = true)
    else:
      return ""
  except:
    return ""

proc printError*(error: CompileError, short: bool = false) =
  if not short:
    stderr.write(absolutePath(error.file))
  else:
    stderr.write(error.file)
  stderr.write(&"({error.line}:{error.col}) ")
  stderr.writeLine($error.kind & ": " & error.message.multiReplace(error.args))
  if not short:
    stderr.write("  | " & repeat(" ", error.col))
    stderr.writeLine(repeat("_", error.len))
    stderr.writeLine("  ?  " & getSourceLine(error.file, error.line))

proc toJson*(filePath: string): JsonNode =
  result = %*[]
  let filePath = absolutePath(filePath)
  for error in errors.errors:
    if absolutePath(error.file) != filePath: continue
    result.add(%*{
      "message": error.message.multiReplace(error.args),
      "line": error.line - 1,
      "column": error.col - 1,
      "len": error.len,
      "pos": error.pos,
      "file": filePath
    })