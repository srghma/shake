/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Incremental

@[expose] public section

/-!
# Incremental build logic for the FM monad.
-/

namespace Shake.Incremental

variable (ℭ : BuildConfig) [BEq ℭ.I] [Hashable ℭ.I] [BEq ℭ.Q] [Hashable ℭ.Q]
variable [∀ q, Hashable (ℭ.R q)]

public inductive FM (ℭ : BuildConfig) (q₀ : ℭ.Q) (α : Type) : Type
  | pure (a : α) : FM ℭ q₀ α
  | input (i : ℭ.I) (k : ℭ.V i → FM ℭ q₀ α) : FM ℭ q₀ α
  | fetch (q : ℭ.Q) (hq : ℭ.rel q q₀) (k : ℭ.R q → FM ℭ q₀ α) : FM ℭ q₀ α

namespace FM

variable {ℭ : BuildConfig} {q₀ : ℭ.Q}

public def bind {α β : Type} (fa : FM ℭ q₀ α) (k : α → FM ℭ q₀ β) : FM ℭ q₀ β :=
  match fa with
  | pure a => k a
  | input i kt => input i (fun v => bind (kt v) k)
  | fetch q hq kt => fetch q hq (fun r => bind (kt r) k)

instance : Monad (FM ℭ q₀) where
  pure := pure
  bind := bind

instance : LawfulMonad (FM ℭ q₀) := LawfulMonad.mk'
  (id_map := fun x => by induction x with
    | pure _ => rfl
    | input i kt ih => exact congrArg (input i) (funext ih)
    | fetch q hq kt ih => exact congrArg (fetch q hq) (funext ih))
  (pure_bind := fun _ _ => rfl)
  (bind_assoc := fun x _ _ => by induction x with
    | pure _ => rfl
    | input i kt ih => exact congrArg (input i) (funext ih)
    | fetch q hq kt ih => exact congrArg (fetch q hq) (funext ih))

public def evalTree {α : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) : FM ℭ q₀ α → α
  | pure a => a
  | input i k => evalTree ι rec (k (ι i))
  | fetch q _ k => evalTree ι rec (k (rec q))

public theorem evalTree_bind {α β : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) (ma : FM ℭ q₀ α)
    (ka : α → FM ℭ q₀ β) :
    evalTree ι rec (bind ma ka) = evalTree ι rec (ka (evalTree ι rec ma)) := by
  induction ma with
  | pure a => rfl
  | input i kt ih => exact ih (ι i)
  | fetch q _ kt ih => exact ih (rec q)

public abbrev pureInput (i : ℭ.I) : FM ℭ q₀ (ℭ.V i) := input i pure

public abbrev pureFetch (q : ℭ.Q) (hq : ℭ.rel q q₀) : FM ℭ q₀ (ℭ.R q) := fetch q hq pure

end FM

public structure InputEntry (ℭ : BuildConfig) where
  i : ℭ.I
  v : ℭ.V i

public structure DepEntry (ℭ : BuildConfig) (q₀ : ℭ.Q) where
  q : ℭ.Q
  hq : ℭ.rel q q₀
  r : ℭ.R q

namespace FM

variable {ℭ : BuildConfig} {q₀ : ℭ.Q}

public def evalTrace_inputs {α : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) :
    FM ℭ q₀ α → List (InputEntry ℭ)
  | pure _ => []
  | input i k => ⟨i, ι i⟩ :: evalTrace_inputs ι rec (k (ι i))
  | fetch q _ k => evalTrace_inputs ι rec (k (rec q))

public def evalTrace_deps {α : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) :
    FM ℭ q₀ α → List (DepEntry ℭ q₀)
  | pure _ => []
  | input i k => evalTrace_deps ι rec (k (ι i))
  | fetch q hq k => ⟨q, hq, rec q⟩ :: evalTrace_deps ι rec (k (rec q))

public theorem evalTrace_inputs_bind {α β : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q)
    (ma : FM ℭ q₀ α) (ka : α → FM ℭ q₀ β) :
    evalTrace_inputs ι rec (bind ma ka) =
      evalTrace_inputs ι rec ma ++ evalTrace_inputs ι rec (ka (evalTree ι rec ma)) := by
  induction ma with
  | pure a => rfl
  | input i kt ih => exact congrArg _ (ih (ι i))
  | fetch q _ kt ih => exact ih (rec q)

public theorem evalTrace_deps_bind {α β : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) (ma : FM ℭ q₀ α)
    (ka : α → FM ℭ q₀ β) :
    evalTrace_deps ι rec (bind ma ka) =
      evalTrace_deps ι rec ma ++ evalTrace_deps ι rec (ka (evalTree ι rec ma)) := by
  induction ma with
  | pure a => rfl
  | input i kt ih => exact ih (ι i)
  | fetch q hq kt ih => exact congrArg _ (ih (rec q))

public theorem evalTrace_deps_value {α : Type} (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) (t : FM ℭ q₀ α) :
    ∀ p ∈ evalTrace_deps ι rec t, p.r = rec p.q := by
  induction t with
  | pure _ => nofun
  | input i k ih => exact ih (ι i)
  | fetch q hq k ih =>
    exact fun
    | _, .head _ => rfl
    | p, .tail _ ht => ih (rec q) p ht

public theorem evalTree_cross {α : Type} (ι ι' : ∀ i, ℭ.V i) (rec rec' : ∀ q, ℭ.R q) (t : FM ℭ q₀ α)
    (hin : ∀ p ∈ evalTrace_inputs ι rec t, ι' p.i = p.v)
    (hdep : ∀ p ∈ evalTrace_deps ι rec t, rec' p.q = p.r) :
    evalTree ι rec t = evalTree ι' rec' t := by
  induction t with
  | pure a => rfl
  | input i k ih =>
    change evalTree ι rec (k (ι i)) = evalTree ι' rec' (k (ι' i))
    rw [hin ⟨i, ι i⟩ (.head _)]
    exact ih (ι i) (fun p hp => hin p (.tail _ hp)) hdep
  | fetch q hq k ih =>
    change evalTree ι rec (k (rec q)) = evalTree ι' rec' (k (rec' q))
    rw [hdep ⟨q, hq, rec q⟩ (.head _)]
    exact ih (rec q) hin (fun p hp => hdep p (.tail _ hp))

public def evalAction (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) :
    MonadAction (FM ℭ q₀) Id where
  rel R t a := R (evalTree ι rec t) a
  rel_pure hab := hab
  rel_bind {_ _ _ _ _ _ ma _ ka _} hma hk := evalTree_bind ι rec ma ka ▸ hk _ _ hma

end FM

variable (ℭ : BuildConfig)

public def tasksTree (tasks : Tasks ℭ) (q₀ : ℭ.Q) : FM ℭ q₀ (ℭ.R q₀) :=
  (tasks q₀).fn (FM ℭ q₀) FM.pureInput FM.pureFetch

public theorem tasksTree_eval (tasks : Tasks ℭ) (q₀ : ℭ.Q) (ι : ∀ i, ℭ.V i) (rec : ∀ q, ℭ.R q) :
    FM.evalTree ι rec (tasksTree ℭ tasks q₀) = (tasks q₀).fn Id ι (fun q _ => rec q) :=
  (tasks q₀).param (FM.evalAction ι rec) FM.pureFetch _ (fun _ => rfl) (fun _ _ => rfl)

public theorem tasksTree_eval_compute (tasks : Tasks ℭ) (q₀ : ℭ.Q) (ι : ∀ i, ℭ.V i) :
    FM.evalTree ι (compute tasks ι) (tasksTree ℭ tasks q₀) = compute tasks ι q₀ := by
  rw [tasksTree_eval]
  conv => rhs; unfold compute

public theorem compute_cross (tasks : Tasks ℭ) (q₀ : ℭ.Q) (ι ι' : ∀ i, ℭ.V i)
    (hin : ∀ p ∈ FM.evalTrace_inputs ι (compute tasks ι) (tasksTree ℭ tasks q₀), ι' p.i = p.v)
    (hdep : ∀ p ∈ FM.evalTrace_deps ι (compute tasks ι) (tasksTree ℭ tasks q₀),
      compute tasks ι' p.q = p.r) :
    compute tasks ι q₀ = compute tasks ι' q₀ := by
  rw [← tasksTree_eval_compute ℭ tasks q₀ ι, FM.evalTree_cross ι ι' _ _ _ hin hdep,
    tasksTree_eval_compute ℭ tasks q₀ ι']

end Shake.Incremental

end
