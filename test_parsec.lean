import Lean.Data.Parsec
open Lean Parsec

def test : Parsec String := do
  let s ← many (satisfy (· != '\n'))
  return s.foldl (·.push ·) ""

def main : IO Unit := do
  match test "hello\n".mkIterator with
  | Except.ok res _ => IO.println s!"Parsed: {res}"
  | Except.error err => IO.println s!"Error: {err}"
