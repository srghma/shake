module

@[expose] public section

namespace Shake.Incremental

class MonadAttach (m : Type → Type) [Monad m] where
  CanReturn : {α : Type} → m α → α → Prop
  attach : {α : Type} → (ma : m α) → m { x // CanReturn ma x }

class LawfulMonadAttach (m : Type → Type) [Monad m] [MonadAttach m] : Prop where
  attach_spec : {α : Type} → (ma : m α) → True

instance [Monad m] : MonadAttach m where
  CanReturn _ _ := True
  attach ma := do
    let x ← ma
    return ⟨x, True.intro⟩

instance [Monad m] : LawfulMonadAttach m where
  attach_spec _ := True.intro

end Shake.Incremental
