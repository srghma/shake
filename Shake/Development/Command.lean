/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development
public import Shake.Incremental

/-!
# Command execution DSL for Shake.
-/

@[expose] public section

namespace Shake.Development

/-- Executes an external command and records dependencies.
    Similar to Haskell Shake's `cmd`. -/
public def cmd (prog : String) (args : List String) : Action Unit := do
  IO.println s!"Executing: {prog} {String.intercalate " " args}"
  let output ← IO.Process.output { cmd := prog, args := args.toArray }
  if output.exitCode != 0 then
    IO.eprintln s!"Command failed with exit code {output.exitCode}"
    IO.eprintln output.stderr
    throw (IO.userError "Command failed")
  pure ()

/-- records a file as an output of the current action. -/
public def produces (file : String) : Action Unit := do
  let s ← get
  set { s with outputs := s.outputs ++ [file] }

end Shake.Development

end
