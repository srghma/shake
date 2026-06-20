/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Std.Data.HashMap

/-!
# Resource cleanup management for Shake.
-/

@[expose] public section

namespace Shake.Internal.Core

open Std (HashMap)

public structure CleanupState where
  unique : Nat
  items : HashMap Nat (IO Unit)

public structure Cleanup where
  ref : IO.Ref CleanupState

public structure ReleaseKey where
  ref : IO.Ref CleanupState
  key : Nat

public def newCleanup : IO (Cleanup × IO Unit) := do
  let ref ← IO.mkRef (CleanupState.mk 0 {})
  let clean := do
    let s ← ref.get
    let mut items := s.items.toList
    -- Sort in reverse order of registration
    items := items.toArray.qsort (fun a b => a.1 > b.1) |>.toList
    for (_, act) in items do
      try
        act
      catch _ =>
        continue
    ref.set (CleanupState.mk 0 {})
  return (Cleanup.mk ref, clean)

public def withCleanup {α : Type} (act : Cleanup → IO α) : IO α := do
  let (c, clean) ← newCleanup
  try
    act c
  finally
    clean

public def register (c : Cleanup) (act : IO Unit) : IO ReleaseKey := do
  let s ← c.ref.get
  let i := s.unique
  c.ref.set (CleanupState.mk (i + 1) (s.items.insert i act))
  return ReleaseKey.mk c.ref i

public def unprotect (rk : ReleaseKey) : IO Unit := do
  let s ← rk.ref.get
  rk.ref.set (CleanupState.mk s.unique (s.items.erase rk.key))

public def release (rk : ReleaseKey) : IO Unit := do
  let s ← rk.ref.get
  match s.items.get? rk.key with
  | some act =>
    rk.ref.set (CleanupState.mk s.unique (s.items.erase rk.key))
    act
  | none => pure ()

public def allocate {α : Type} (c : Cleanup) (acquire : IO α) (rel : α → IO Unit) : IO α := do
  let v ← acquire
  let _ ← register c (rel v)
  return v

end Shake.Internal.Core

end
