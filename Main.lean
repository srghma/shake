/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development
public import Lean

/-!
# lshake executable entry point.
-/

@[expose] public section

open Shake.Development

public def main (args : List String) : IO Unit := do
  match args with
  | [shakeFile] =>
    IO.println s!"Running lshake with {shakeFile}"
  | _ =>
    IO.println "Usage: lshake <Shake.lean>"

end
