module

public import Shake.Incremental
public import Shake.Internal.Core.Binary
public import Std.Data.HashMap
public import Std.Data.DHashMap

@[expose] public section

namespace Shake.Incremental

open Std (DHashMap HashMap)
open Shake.Internal.Core (Binary)

namespace ShakeRT

public structure Memo (ℭ : BuildConfig) (q₀ : ℭ.Q) [BEq (BuildConfig.Q ℭ)] [Hashable (BuildConfig.Q ℭ)] [BEq (BuildConfig.I ℭ)] [Hashable (BuildConfig.I ℭ)] where
  value : ℭ.R q₀
  queryDeps : HashMap (BuildConfig.Q ℭ) UInt64
  inputDeps : HashMap (BuildConfig.I ℭ) UInt64
  hash : UInt64

public structure Store (ℭ : BuildConfig) (J : Type) [BEq (BuildConfig.Q ℭ)] [Hashable (BuildConfig.Q ℭ)] [BEq (BuildConfig.I ℭ)] [Hashable (BuildConfig.I ℭ)] where
  inputs : J
  memos : DHashMap (BuildConfig.Q ℭ) (fun q => Memo ℭ q)

section
  variable {ℭ : BuildConfig} {J : Type} [BEq (BuildConfig.Q ℭ)] [Hashable (BuildConfig.Q ℭ)] [BEq (BuildConfig.I ℭ)] [Hashable (BuildConfig.I ℭ)] [Binary (BuildConfig.Q ℭ)] [Binary (BuildConfig.I ℭ)]

  public def serializeMemo {q₀ : ℭ.Q} [Binary (ℭ.R q₀)] (m : Memo ℭ q₀) : ByteArray :=
    Binary.put m.value ++ Binary.put m.queryDeps.toList ++ Binary.put m.inputDeps.toList ++ Binary.put m.hash

  public def deserializeMemo {q₀ : ℭ.Q} [Binary (ℭ.R q₀)] (b : ByteArray) : Option (Memo ℭ q₀ × ByteArray) := do
    let (value, b1) ← Binary.get (α := ℭ.R q₀) b
    let (queryDepsList, b2) ← Binary.get (α := List (ℭ.Q × UInt64)) b1
    let (inputDepsList, b3) ← Binary.get (α := List (ℭ.I × UInt64)) b2
    let (hash, b4) ← Binary.get (α := UInt64) b3
    return (⟨value, HashMap.ofList queryDepsList, HashMap.ofList inputDepsList, hash⟩, b4)

  public def serializeStore [Binary J] [∀ q, Binary (ℭ.R q)] (s : Store ℭ J) : ByteArray :=
    let memoList := s.memos.toList.map fun (⟨q, m⟩ : (a : ℭ.Q) × Memo ℭ a) => (q, serializeMemo m)
    Binary.put s.inputs ++ Binary.put memoList

  public def deserializeStore [Binary J] [∀ q, Binary (ℭ.R q)] (b : ByteArray) : Option (Store ℭ J × ByteArray) := do
    let (inputs, b1) ← Binary.get (α := J) b
    let (memoList, b2) ← Binary.get (α := List (ℭ.Q × ByteArray)) b1
    let mut memos : DHashMap ℭ.Q (fun q => Memo ℭ q) := {}
    for (q, mb) in memoList do
       match deserializeMemo (q₀ := q) mb with
       | some (m, _) => memos := memos.insert q m
       | none => continue
    return (⟨inputs, memos⟩, b2)
end

end ShakeRT

end Shake.Incremental
