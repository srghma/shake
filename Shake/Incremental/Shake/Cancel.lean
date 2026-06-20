/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental.Shake.Basic

/-!
# Cancellation support for Shake runner.
-/

@[expose] public section

namespace Shake.Incremental

variable {ℭ : BuildConfig} {J : Type} [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I]

instance : MonadCancel (Shake ℭ J) where
  cancel := pure ()

end Shake.Incremental

end
