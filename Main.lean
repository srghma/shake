module

public import Shake.Development
public import Lean

@[expose] public section

open Shake.Development
def main (args : List String) : IO Unit := do
  match args with
  | [shakeFile] =>
    IO.println s!"Running lshake with {shakeFile}"
  | _ =>
    IO.println "Usage: lshake <Shake.lean>"
