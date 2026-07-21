# KOVYL

**KOVYL** is a statically typed programming language with manual memory management.

The language emphasizes explicitness of operations — with no hidden runtime behavior or garbage collector.

KOVYL offers a rich type system, functions as first-class objects, array operations, as well as utilities for string manipulation and formatting. Both procedural and functional programming paradigms are supported.

The language syntax is designed to be readable and unambiguous.

**Syntax example:**

```kovyl
func string greeting(string name) do 
  return fmt:("Hello from Kovyl, ", name, "!")
end

string[] names = {v"Alice", v"Ben", v"John"}

for name = names do
  print:(greeting(name))
end
```
