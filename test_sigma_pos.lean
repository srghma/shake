import Std.Internal.Parsec
open Std.Internal.Parsec

def main : IO Unit := do
  let s := "hello"
  let it : Sigma String.Pos := ⟨s, s.startPos⟩
  IO.println s!"It works: {it.1}"
