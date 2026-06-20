import Lean
open Lean
def main : IO Unit := do
  IO.println s!"String.Pos: {Lean.runMeta (Meta.ppExpr (Expr.const ``String.Pos []))}"
