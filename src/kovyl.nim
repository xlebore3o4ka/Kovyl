import core/[parser]

proc main() =
  let text = "1000 / 123 + 5\n12 - 1 * 0"
  var parser = newParser(text, "<string>")

when isMainModule:
  main()