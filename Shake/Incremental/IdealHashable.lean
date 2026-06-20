/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental
public import Mathlib.Logic.Embedding.Basic

/-!
# IdealHashable property for Shake.
-/

@[expose] public section

namespace Shake.Incremental

public class IdealHashable (α : Type) [Hashable α] : Prop where
  inj : Function.Injective (hash : α → UInt64)

@[instance] public axiom Hashable.ideal {α : Type} [Hashable α] : IdealHashable α

@[inline] public def Hashable.toEmbedding {α : Type} [Hashable α] [IdealHashable α] :
    α ↪ UInt64 :=
  ⟨hash, IdealHashable.inj⟩

end Shake.Incremental

end
