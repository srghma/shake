/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

/-!
# General utilities for Shake.
-/

@[expose] public section

namespace Shake.Development

public def getDirectoryFiles (dir : String) (patterns : List String) : IO (List String) := do
  IO.println s!"Finding files in {dir} matching {patterns}"
  pure []

end Shake.Development

end
