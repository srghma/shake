/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental
public import Shake.Internal.Core.Binary

@[expose] public section

/-!
# Storage structures for incremental builds.
-/

namespace Shake.Incremental

open Std (DHashMap HashMap)
open Shake.Internal.Core

variable (ℭ : BuildConfig) [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I]

public structure Memo (q₀ : ℭ.Q) where
  value : ℭ.R q₀
  queryDeps : HashMap ℭ.Q UInt64
  inputDeps : HashMap ℭ.I UInt64
  hash : UInt64

public structure Store (J : Type) where
  inputs : J
  memos : DHashMap ℭ.Q (fun q => Memo ℭ q)

variable {ℭ} {J : Type} [Binary ℭ.Q] [Binary ℭ.I]

public def serializeMemo {q₀ : ℭ.Q} [Binary (ℭ.R q₀)] (m : Memo ℭ q₀) : ByteArray :=
  Binary.put m.value ++ Binary.put m.queryDeps.toList ++ Binary.put m.inputDeps.toList ++
    Binary.put m.hash

public def deserializeMemo {q₀ : ℭ.Q} [Binary (ℭ.R q₀)] (b : ByteArray) :
    Option (Memo ℭ q₀ × ByteArray) := do
  let (value, b1) ← Binary.get (α := ℭ.R q₀) b
  let (queryDepsList, b2) ← Binary.get (α := List (ℭ.Q × UInt64)) b1
  let (inputDepsList, b3) ← Binary.get (α := List (ℭ.I × UInt64)) b2
  let (hash, b4) ← Binary.get (α := UInt64) b3
  return (⟨value, HashMap.ofList queryDepsList, HashMap.ofList inputDepsList, hash⟩, b4)

public def serializeStore [Binary J] [∀ q, Binary (ℭ.R q)] (s : Store ℭ J) : ByteArray :=
  let memoList := s.memos.toList.map fun ⟨q, m⟩ => (q, serializeMemo m)
  Binary.put s.inputs ++ Binary.put memoList

public def deserializeStore [Binary J] [∀ q, Binary (ℭ.R q)] (b : ByteArray) :
    Option (Store ℭ J × ByteArray) := do
  let (inputs, b1) ← Binary.get (α := J) b
  let (memoList, b2) ← Binary.get (α := List (ℭ.Q × ByteArray)) b1
  let mut memos : DHashMap ℭ.Q (fun q => Memo ℭ q) := { }
  for (q, mb) in memoList do
    match deserializeMemo (q₀ := q) mb with
    | some (m, _) =>
      memos := memos.insert q m
    | none =>
      continue
  return (⟨inputs, memos⟩, b2)

end Shake.Incremental

end
