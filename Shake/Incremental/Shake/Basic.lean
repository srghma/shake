/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental.Basic
public import Shake.Incremental.IdealHashable
public import Shake.Incremental.MonadCancel
public import Shake.Incremental.Store

/-!
# Shake incremental build runner.
-/

@[expose] public section

namespace Shake.Incremental

open Shake.Incremental

variable {ℭ : BuildConfig} {J : Type} [Input ℭ J] (tasks : Tasks ℭ)
variable [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I]
variable [∀ i, Hashable (ℭ.V i)] [∀ i, IdealHashable (ℭ.V i)]
variable [∀ q, Hashable (ℭ.R q)] [∀ q, IdealHashable (ℭ.R q)]

public abbrev Shake (ℭ : BuildConfig) (J : Type) [BEq ℭ.Q] [Hashable ℭ.Q]
    [BEq ℭ.I] [Hashable ℭ.I] :=
  StateT (Store ℭ J) IO

public def runShake {ℭ : BuildConfig} {J : Type} [BEq ℭ.Q] [Hashable ℭ.Q]
    [BEq ℭ.I] [Hashable ℭ.I] (inputs : J) (m : Shake ℭ J α) :
    IO (α × Store ℭ J) :=
  m.run ⟨inputs, {}⟩

end Shake.Incremental

end
