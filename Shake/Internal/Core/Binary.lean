/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

@[expose] public section

/-!
# Binary serialization and deserialization for Lean 4.
-/

namespace Shake.Internal.Core

/-- Binary type class for serialization. -/
public class Binary (α : Type) where
  put : α → ByteArray
  get : ByteArray → Option (α × ByteArray)

instance : Binary UInt64 where
  put n :=
    let b0 := (n &&& 0xFF).toUInt8
    let b1 := ((n >>> 8) &&& 0xFF).toUInt8
    let b2 := ((n >>> 16) &&& 0xFF).toUInt8
    let b3 := ((n >>> 24) &&& 0xFF).toUInt8
    let b4 := ((n >>> 32) &&& 0xFF).toUInt8
    let b5 := ((n >>> 40) &&& 0xFF).toUInt8
    let b6 := ((n >>> 48) &&& 0xFF).toUInt8
    let b7 := ((n >>> 56) &&& 0xFF).toUInt8
    ByteArray.mk #[b0, b1, b2, b3, b4, b5, b6, b7]
  get b :=
    if b.size < 8 then none
    else
      let b0 := b.data.getD 0 0 |>.toUInt64
      let b1 := b.data.getD 1 0 |>.toUInt64
      let b2 := b.data.getD 2 0 |>.toUInt64
      let b3 := b.data.getD 3 0 |>.toUInt64
      let b4 := b.data.getD 4 0 |>.toUInt64
      let b5 := b.data.getD 5 0 |>.toUInt64
      let b6 := b.data.getD 6 0 |>.toUInt64
      let b7 := b.data.getD 7 0 |>.toUInt64
      let n := b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24) |||
               (b4 <<< 32) ||| (b5 <<< 40) ||| (b6 <<< 48) ||| (b7 <<< 56)
      some (n, ByteArray.mk (b.data.extract 8 b.size))

instance : Binary ByteArray where
  put b := Binary.put b.size.toUInt64 ++ b
  get b := match Binary.get (α := UInt64) b with
    | some (n, rest) =>
      let nNat := n.toNat
      if rest.size < nNat then none
      else some (ByteArray.mk (rest.data.extract 0 nNat),
                 ByteArray.mk (rest.data.extract nNat rest.size))
    | none => none

instance : Binary String where
  put s := Binary.put s.toUTF8
  get b :=
    match Binary.get (α := ByteArray) b with
    | some (bytes, rest) =>
      match String.fromUTF8? bytes with
      | some s => some (s, rest)
      | none => none
    | none => none

instance [Binary α] : Binary (List α) where
  put l :=
    let count := (l.length.toUInt64)
    Binary.put count ++ l.foldl (fun acc x => acc ++ Binary.put x) ByteArray.empty
  get b :=
    match Binary.get (α := UInt64) b with
    | some (n, rest) =>
      let rec go (count : Nat) (curr : ByteArray) (acc : List α) : Option (List α × ByteArray) :=
        match count with
        | 0 => some (acc.reverse, curr)
        | c + 1 => match Binary.get (α := α) curr with
             | some (x, next) => go c next (x :: acc)
             | none => none
      go n.toNat rest []
    | none => none

instance [Binary α] [Binary β] : Binary (α × β) where
  put p := Binary.put p.1 ++ Binary.put p.2
  get b := match Binary.get (α := α) b with
    | some (a, r1) => match Binary.get (α := β) r1 with
      | some (b', r2) => some ((a, b'), r2)
      | none => none
    | none => none

end Shake.Internal.Core

end
