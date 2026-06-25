import ../core/errors
import std/[tables, strutils]

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

proc printError*(error: CompileError) =
  var msg = error.message
  for key, value in error.args:
    msg = msg.replace(key, value)

  stderr.writeLine("Error [", $error.kind, "]: ", msg)
  stderr.writeLine("  --> ", error.file, ":", error.line, ":", error.col + 1)

  stderr.writeLine("   |")
  stderr.write(" ", error.line, " | ")

  let sourceLine = getSourceLine(error.file, error.line)
  stderr.writeLine(sourceLine)

  stderr.write("   | ")
  for i in 0 ..< error.col - 1:
    stderr.write(" ")
  for i in 0 ..< error.len:
    stderr.write("^")
  stderr.writeLine("")