# Package

version       = "0.0.0"
author        = "Thinkeater Studio"
description   = "Raw programming language Kovyl"
license       = "LGPL-3.0"

srcDir        = "src"
binDir        = "build"
bin           = @["kovyl", "linter"]

# Dependencies

requires "nim >= 2.0.0"