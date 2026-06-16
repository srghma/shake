import Shake.Development
import Shake.Development.FilePath

open Shake.Development
open Shake.Development.FilePath

def main : IO Unit := shakeArgs shakeOptions do
  want ["_build/main"]

  phis "_build/main" fun out => do
    let obj := "_build/main.o"
    need [obj]
    cmd_ "gcc" ["-o", out, obj]

  phis "_build/*.o" fun out => do
    let cFile := dropDirectory1 (replaceExtension out "c")
    need [cFile]
    cmd_ "gcc" ["-c", cFile, "-o", out]
