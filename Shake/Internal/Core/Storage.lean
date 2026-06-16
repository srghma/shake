module

public import Shake.Incremental
public import Shake.Internal.Core.Binary
public import Shake.Incremental.Store
public import Std.Data.HashMap

@[expose] public section

namespace Shake.Internal.Core

open Shake.Incremental (BuildConfig)
open Shake.Incremental.ShakeRT

public structure Storage where
  version : String := "SHAKE-LEAN-0.1"
  path : String

section
  variable (ℭ : BuildConfig) {J : Type}
    [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I]
    [Binary J] [Binary ℭ.Q] [Binary ℭ.I] [∀ q, Binary (ℭ.R q)]

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
        else return none
      | none => return none
    else return none
end

end Shake.Internal.Core
