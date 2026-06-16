import Shake.Incremental.Store
namespace Incremental
open Std (HashMap)
variable
  (ℭ : BuildConfig)
  (J : Type) [Input ℭ J]
  [BEq ℭ.I] [Hashable ℭ.I] [∀ i, Hashable (ℭ.V i)]
  [BEq ℭ.Q] [Hashable ℭ.Q] [∀ q, Hashable (ℭ.R q)]
def run (tasks : Tasks ℭ) (ι₀ : ∀ i, ℭ.V i) (q₀ : ℭ.Q) : IO (ℭ.R q₀) := do
  pure (compute tasks ι₀ q₀)
end Incremental
