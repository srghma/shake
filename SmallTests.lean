import Shake.Development.Ninja.Lexer
import Shake.Development.Ninja.Parse
import Shake.Internal.Core.Extra
import Shake.Internal.Core.Cleanup

def main : IO Unit := do
  IO.println "Testing Ninja Lexer..."
  let s := "var = val\nbuild out: rule dep\n"
  let res := Shake.Development.Ninja.lexer s
  IO.println s!"Lexer result: {repr res}"

  IO.println "Testing Ninja Parser..."
  let content := "rule cc\n  command = gcc $in -o $out\nbuild app: cc main.c\n"
  let tmpFile := "test_small.ninja"
  IO.FS.writeFile tmpFile content
  let env ← Shake.Development.Ninja.newEnv
  let ninja ← Shake.Development.Ninja.parse tmpFile env
  if ninja.rules.length == 1 && ninja.singles.length == 1 then
    IO.println "Small Ninja Parser test passed."
  else
    IO.eprintln "Small Ninja Parser test failed."
  IO.FS.removeFile tmpFile

  IO.println "Testing Cleanup..."
  let ref ← IO.mkRef (0 : Nat)
  Shake.Internal.Core.withCleanup (fun c => do
    let _ ← Shake.Internal.Core.register c (ref.modify (· + 1))
    let _ ← Shake.Internal.Core.register c (ref.modify (· + 10))
  )
  if (← ref.get) == 11 then
    IO.println "Small Cleanup test passed."
  else
    IO.eprintln "Small Cleanup test failed."
