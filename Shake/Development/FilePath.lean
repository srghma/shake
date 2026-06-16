module

@[expose] public section

namespace Shake.Development.FilePath
def dropDirectory1 (path : String) : String :=
  let parts := path.splitOn "/"
  match parts with
  | _ :: rest => String.intercalate "/" rest
  | [] => ""
def takeDirectory1 (path : String) : String :=
  let parts := path.splitOn "/"
  match parts with
  | first :: _ => first
  | [] => ""
def replaceDirectory1 (path : String) (dir : String) : String :=
  dir ++ "/" ++ dropDirectory1 path
def isPathSeparator (c : Char) : Bool :=
  c == '/' || c == '\\'
def dropExtension (path : String) : String :=
  let rec findDot (chars : List Char) : Option (List Char) :=
    match chars with
    | '.' :: rest => some rest.reverse
    | _ :: rest => findDot rest
    | [] => none
  match findDot path.toList.reverse with
  | some base => String.ofList base
  | none => path
def replaceExtension (path : String) (ext : String) : String :=
  dropExtension path ++ "." ++ ext
def combineExtension (path : String) (ext : String) : String :=
  replaceExtension path ext
infixr:60 " -<.> " => combineExtension
end Shake.Development.FilePath
