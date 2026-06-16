import Shake.Incremental
import Std.Data.HashMap
namespace Incremental
open Std (HashMap)
namespace ShakeRT
structure Memo (ℭ : BuildConfig) (q₀ : ℭ.Q) [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I] where
  value : ℭ.R q₀
  queryDeps : HashMap ℭ.Q UInt64
  inputDeps : HashMap ℭ.I UInt64
  hash : UInt64
structure Store (ℭ : BuildConfig) (J : Type) [BEq ℭ.Q] [Hashable ℭ.Q] [BEq ℭ.I] [Hashable ℭ.I] where
  inputs : J
  memos : HashMap ℭ.Q (Σ q₀ : ℭ.Q, Memo ℭ q₀)
end ShakeRT
end Incremental
