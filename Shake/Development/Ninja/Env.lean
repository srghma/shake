/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Std.Data.HashMap

/-!
# A Ninja style environment, equivalent to a non-empty list of mutable hash tables.
-/

@[expose] public section

namespace Shake.Development.Ninja

open Std (HashMap)

public inductive Env (k v : Type) [BEq k] [Hashable k] where
  | mk (ref : IO.Ref (HashMap k v)) (parent : Option (Env k v))

public def newEnv [BEq k] [Hashable k] : IO (Env k v) := do
  let ref ← IO.mkRef {}
  pure (Env.mk ref none)

public def scopeEnv [BEq k] [Hashable k] (e : Env k v) : IO (Env k v) := do
  let ref ← IO.mkRef {}
  pure (Env.mk ref (some e))

public def addEnv [BEq k] [Hashable k] (e : Env k v) (key : k) (val : v) : IO Unit :=
  match e with
  | Env.mk ref _ => ref.modify (fun m => m.insert key val)

public def askEnv [BEq k] [Hashable k] (e : Env k v) (key : k) : IO (Option v) :=
  match e with
  | Env.mk ref parent => do
    let m ← ref.get
    match m.get? key with
    | some val => pure (some val)
    | none =>
      match parent with
      | some p => askEnv p key
      | none => pure none

public def fromEnv [BEq k] [Hashable k] (e : Env k v) : IO (HashMap k v) :=
  match e with
  | Env.mk ref _ => ref.get

end Shake.Development.Ninja

end
