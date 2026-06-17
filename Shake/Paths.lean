/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

/-!
# Fake cabal module for local building
-/

@[expose] public section

namespace Shake.Paths

/-- If Shake can't find files in the data directory it tries relative to the executable -/
public def getDataDir : IO System.FilePath :=
  pure "random_path_that_cannot_possibly_exist"

public def version : List Nat := [0, 1, 0]

end Shake.Paths

end
