import Std.Internal.Parsec
open Std.Internal.Parsec

def test : String.Parser String := do
  manyChars (satisfy (· != '\n'))

def main : IO Unit := do
  let s := "hello\n"
  match test ⟨s, s.startPos⟩ with
  | .success _ res => IO.println s!"Parsed: {res}"
  | .error _ err => IO.println s!"Error: {err}"
