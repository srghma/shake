module

@[expose] public section

namespace Shake.Development

inductive Pat where
  | Lit (s : String)
  | Star
  | Skip
  deriving BEq, Repr

def parse (p : String) : List Pat :=
  let parts := p.splitOn "/"
  parts.map fun part =>
    if part == "**" then Pat.Skip
    else if part == "*" then Pat.Star
    else Pat.Lit part

partial def matchPath (pats : List Pat) (path : List String) : Bool :=
  match pats, path with
  | [], [] => true
  | Pat.Skip :: ps, path =>
    matchPath ps path || (match path with | [] => false | _ :: ss => matchPath (Pat.Skip :: ps) ss)
  | Pat.Star :: ps, _ :: ss => matchPath ps ss
  | Pat.Lit l :: ps, s :: ss => l == s && matchPath ps ss
  | _, _ => false

def filePatternMatch (pat : String) (path : String) : Bool :=
  matchPath (parse pat) (path.splitOn "/")

end Shake.Development
