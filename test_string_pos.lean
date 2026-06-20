def main : IO Unit := do
  let s := "hello"
  let p : String.Pos s := s.startPos
  IO.println s!"{p.1}"
