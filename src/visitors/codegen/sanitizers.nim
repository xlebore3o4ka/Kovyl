import std/[sets, strformat]

const 
  debugFlag = "KOVYDEBUG"

  panicZeroDivision = "ZeroDivision"

var panicFuncname = "kovypanic"

proc generatePanicSystem*(): string =
  let prefix = panicFuncname

  fmt"""


typedef struct {{
  const char* kind;
  const char* message;
}} {prefix}_Panic;

typedef bool (*{prefix}_PanicHandler_t)(const {prefix}_Panic*);
static {prefix}_PanicHandler_t {prefix}_PanicHandler = NULL;

void {prefix}_setPanicHandler({prefix}_PanicHandler_t handler) {{
  {prefix}_PanicHandler = handler;
}}
void {prefix}(const char* kind, const char* message, int64_t line) {{
  {prefix}_Panic p = {{ kind, message }};
  if ({prefix}_PanicHandler != NULL) {{if ({prefix}_PanicHandler(&p)) return;}}
  fprintf(stderr, "Panic at line %lld: %s [%s]\n", (long long)line, message, kind);
  exit(1);
}}

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

proc sanitizerIncludes*(): HashSet[string] = 
  ["<stdbool.h>", "<stdint.h>", "<stdio.h>"].toHashSet