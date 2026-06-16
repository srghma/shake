import Shake.Development.FilePath
import Shake.Development.Util
import Shake.Incremental
import Shake.Incremental.Store
import Std.Data.HashMap
namespace Shake.Development
open Shake.Development.FilePath
open Std (HashMap)
abbrev Action (α : Type) : Type := IO α
abbrev Rules (α : Type) : Type := StateT (HashMap String (String -> Action Unit)) IO α
structure ShakeOptions where
  shakeFiles : String := "_build"
def shakeOptions : ShakeOptions := {}
def want (_targets : List String) : Rules Unit := do pure ()
def need (_files : List String) : Action Unit := do pure ()
def phis (pattern : String) (action : String -> Action Unit) : Rules Unit := do
  let m ← get
  set (m.insert pattern action)
infixr:60 " %> " => phis
def cmd_ (prog : String) (args : List String) : Action Unit := do
  let _ ← IO.Process.output { cmd := prog, args := args.toArray }
  pure ()
def shakeArgs (_opts : ShakeOptions) (rules : Rules Unit) : IO Unit := do
  let (_, _ruleMap) ← rules.run {}
  pure ()
end Shake.Development
