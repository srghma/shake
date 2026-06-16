/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental

/-!
# MonadCancel property for Shake.
-/

@[expose] public section

namespace Shake.Incremental

public class MonadCancel (m : Type → Type) [Monad m] where
  cancel : m Unit

end Shake.Incremental

end
