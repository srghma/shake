/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental.Store

/-!
# Persistent storage management for Shake.
-/

@[expose] public section

namespace Shake.Internal.Core

open Shake.Incremental

public structure Storage where
  path : System.FilePath
  version : String

variable {ℭ : BuildConfig} {J : Type} [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I]
variable [Binary J] [Binary ℭ.Q] [Binary ℭ.I] [∀ q, Binary (ℭ.R q)]

public def saveStore (storage : Storage) (s : Store ℭ J) : IO Unit := do
  let content := serializeStore s
  let fullData := Binary.put storage.version ++ content
  IO.FS.writeBinFile storage.path fullData

public def loadStore (storage : Storage) : IO (Option (Store ℭ J)) := do
  if ← System.FilePath.pathExists storage.path then
    let data ← IO.FS.readBinFile storage.path
    match Binary.get (α := String) data with
    | some (v, rest) =>
      if v == storage.version then
        match deserializeStore (ℭ := ℭ) rest with
        | some (s, _) => return some s
        | none => return none
      else
        return none
    | none => return none
  else
    return none

end Shake.Internal.Core
