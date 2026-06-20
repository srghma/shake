import Lean
def main : IO Unit := do
  let s := "hello"
  let p : String.Pos := ⟨0⟩
  IO.println s!"{p.byteIdx}"
