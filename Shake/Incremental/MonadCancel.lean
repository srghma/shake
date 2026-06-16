module

@[expose] public section

namespace Shake.Incremental

class MonadCancel (m : Type → Type) [Monad m] where
  CanCancel : Bool
  checkpoint : m Unit

end Shake.Incremental
