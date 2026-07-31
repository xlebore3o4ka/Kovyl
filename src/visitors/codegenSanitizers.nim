const 
  debugFlag = "KOVYDEBUG"

  panicZeroDivision = "ZeroDivision"

var panicFuncname = "panic"

proc generatePanicSystem*(funcname: string): string =
  panicFuncname = funcname
  """

typedef struct {
  const char* kind;
  const char* message;
} """ & funcname & """_Panic;

typedef bool (*""" & funcname & """_PanicHandler_t)(const """ & funcname & """_Panic*);
static """ & funcname & """_PanicHandler_t """ & funcname & """_PanicHandler = NULL;

void """ & funcname & """_setPanicHandler(""" & funcname & """_PanicHandler_t handler) {
  """ & funcname & """_PanicHandler = handler;
}
void """ & funcname & """(const char* kind, const char* message, int64_t line) {
  """ & funcname & """_Panic p = { kind, message };
  if (""" & funcname & """_PanicHandler != NULL) {if (""" & funcname & """_PanicHandler(&p)) return;}
  fprintf(stderr, "Panic at line %lld: %s [%s]\n", (long long)line, message, kind);
  exit(1);
}

  """

proc generateDebugDefine*(release: bool): string =
  "#define KOVYDEBUG " & (if release: "0" else: "1")

proc generatePanicCall(kind: string, message: string, line: Positive): string =
  panicFuncname & "(\"" & kind & "\", \"" & message & "\", " & $line & ");"

proc generateZeroDivisionSanitizer*(temp: string, line: Positive): string =
  result = "#if " & debugFlag & "\n"
  result &= "  if (" & temp & " == 0) {"
  result &= generatePanicCall(panicZeroDivision, "Zero division", line)
  result &= "}\n#endif"