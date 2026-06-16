/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

/-!
# FilePath utilities for Shake.
-/

@[expose] public section

namespace Shake.Development.FilePath

public def dropDirectory1 (path : String) : String :=
  let parts := path.splitOn "/"
  match parts with
  | _ :: rest => String.intercalate "/" rest
  | [] => ""

public def takeDirectory1 (path : String) : String :=
  let parts := path.splitOn "/"
  match parts with
  | first :: _ => first
  | [] => ""

public def replaceDirectory1 (path : String) (dir : String) : String :=
  dir ++ "/" ++ dropDirectory1 path

public def isPathSeparator (c : Char) : Bool :=
  c == '/' || c == '\\'

public def dropExtension (path : String) : String :=
  let rec findDot (chars : List Char) : Option (List Char) :=
    match chars with
    | '.' :: rest => some rest.reverse
    | _ :: rest => findDot rest
    | [] => none
  match findDot path.toList.reverse with
  | some base => String.ofList base
  | none => path

public def replaceExtension (path : String) (ext : String) : String :=
  dropExtension path ++ "." ++ ext

public def combineExtension (path : String) (ext : String) : String :=
  replaceExtension path ext

infixr:60 " -<.> " => combineExtension

end Shake.Development.FilePath

end
