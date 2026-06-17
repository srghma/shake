/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Std.Data.HashMap

/-!
# General utility functions for Shake.
-/

@[expose] public section

namespace Shake.Internal.Core

/-- If a string has any spaces then put quotes around and double up all internal quotes. -/
public def wrapQuote (s : String) : String :=
  if s.any (· == ' ') then
    "\"" ++ String.join (s.toList.map (fun c => if c == '\"' then "\"\"" else String.singleton c)) ++ "\""
  else
    s

/-- If a string has any spaces then put brackets around it. -/
public def wrapBracket (s : String) : String :=
  if s.any (· == ' ') then "(" ++ s ++ ")" else s

/-- unconcat [[a]] [b] -> [[b]] -/
public def unconcat {α β : Type} : List (List α) → List β → List (List β)
  | [], _ => []
  | (a::as), bs =>
    let (b1, b2) := bs.splitAt a.length
    b1 :: unconcat as b2

/-- zipWithExacts: unequal lengths results in an error (or Option in safe version) -/
public def zipWithExact? {α β γ : Type} (f : α → β → γ) : List α → List β → Option (List γ)
  | [], [] => some []
  | (a::as), (b::bs) => do
    let res ← zipWithExact? f as bs
    return f a b :: res
  | _, _ => none

public def zipExact? {α β : Type} : List α → List β → Option (List (α × β)) :=
  zipWithExact? (·, ·)

/-- forNothingM in Haskell: returns Nothing if any f x returns Nothing, else Just [b] -/
public def forNothingM {m : Type → Type} [Monad m] {α β : Type} (xs : List α)
    (f : α → m (Option β)) : m (Option (List β)) := do
  match xs with
  | [] => return some []
  | x::xs =>
    match ← f x with
    | none => return none
    | some v =>
      match ← forNothingM xs f with
      | none => return none
      | some vs => return some (v :: vs)

/-- IO utilities -/

public def doesFileExist_ (path : System.FilePath) : IO Bool := do
  try
    System.FilePath.pathExists path
  catch _ =>
    return false

public def doesDirectoryExist_ (path : System.FilePath) : IO Bool := do
  try
    System.FilePath.pathExists path
  catch _ =>
    return false

public def removeFile_ (path : System.FilePath) : IO Unit := do
  try
    IO.FS.removeFile path
  catch _ =>
    return ()

public def createDirectoryRecursive (path : System.FilePath) : IO Unit := do
  if !(← doesFileExist_ path) then
    IO.FS.createDirAll path

end Shake.Internal.Core

end
