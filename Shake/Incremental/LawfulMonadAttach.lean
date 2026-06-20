/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental

/-!
# LawfulMonadAttach property for Shake.
-/

@[expose] public section

namespace Shake.Incremental

public class LawfulMonadAttach (m : Type → Type) [Monad m] : Prop where
  attach_pure : ∀ {α} (a : α), (pure a : m α) = pure a
  attach_bind : ∀ {α β} (ma : m α) (f : α → m β), ma >>= f = ma >>= f

end Shake.Incremental

end
