/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Std.Data.HashMap
public import Shake.Development.Classes

/-!
# String and value interning for Shake.
-/

@[expose] public section

namespace Shake.Internal.Core

open Std (HashMap)

public structure Id where
  val : UInt32
  deriving BEq, Hashable, Repr, Ord

instance : Binary Id where
  put i := Binary.put i.val.toUInt64
  get b := match Binary.get (α := UInt64) b with
    | some (v, rest) => some (⟨v.toUInt32⟩, rest)
    | none => none

public structure Intern (α : Type) [BEq α] [Hashable α] where
  maxId : UInt32
  map : HashMap α Id

public def emptyIntern [BEq α] [Hashable α] : Intern α :=
  ⟨0, {}⟩

public def insertIntern [BEq α] [Hashable α] (k : α) (v : Id) (intern : Intern α) : Intern α :=
  ⟨max intern.maxId v.val, intern.map.insert k v⟩

public def addIntern [BEq α] [Hashable α] (k : α) (intern : Intern α) : Intern α × Id :=
  let newId := ⟨intern.maxId + 1⟩
  (⟨newId.val, intern.map.insert k newId⟩, newId)

public def lookupIntern [BEq α] [Hashable α] (k : α) (intern : Intern α) : Option Id :=
  intern.map.get? k

public def toListIntern [BEq α] [Hashable α] (intern : Intern α) : List (α × Id) :=
  intern.map.toList

public def fromListIntern [BEq α] [Hashable α] (xs : List (α × Id)) : Intern α :=
  let maxId := xs.foldl (fun acc (_, id) => max acc id.val) 0
  ⟨maxId, HashMap.ofList xs⟩

end Shake.Internal.Core

end
