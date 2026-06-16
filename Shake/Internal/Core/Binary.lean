module

@[expose] public section

namespace Shake.Internal.Core

/-- Binary type class for serialization. -/
class Binary (α : Type) where
  put : α → ByteArray
  get : ByteArray → Option (α × ByteArray)

instance : Binary String where
  put s := s.toUTF8
  get b := some (String.fromUTF8! b, ByteArray.empty)

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
      let n := b[0]!.toUInt64 ||| (b[1]!.toUInt64 <<< 8) |||
               (b[2]!.toUInt64 <<< 16) ||| (b[3]!.toUInt64 <<< 24) |||
               (b[4]!.toUInt64 <<< 32) ||| (b[5]!.toUInt64 <<< 40) |||
               (b[6]!.toUInt64 <<< 48) ||| (b[7]!.toUInt64 <<< 56)
      some (n, ByteArray.mk (b.data.extract 8 b.size))

instance : Binary ByteArray where
  put b := Binary.put b.size.toUInt64 ++ b
  get b := match Binary.get (α := UInt64) b with
    | some (n, rest) =>
      if rest.size < n.toNat then none
      else some (ByteArray.mk (rest.data.extract 0 n.toNat),
                 ByteArray.mk (rest.data.extract n.toNat rest.size))
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
