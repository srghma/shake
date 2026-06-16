/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

@[expose] public section

namespace Shake.Development

public inductive Pat where
  | Lit (s : String)
  | Star
  | Skip
  deriving BEq, Repr

public def parse (p : String) : List Pat :=
  let parts := p.splitOn "/"
  parts.map fun part =>
    if part == "**" then Pat.Skip
    else if part == "*" then Pat.Star
    else Pat.Lit part

public partial def matchPath (pats : List Pat) (path : List String) : Bool :=
  match pats, path with
  | [], [] => true
  | Pat.Skip :: ps, path =>
    matchPath ps path || (match path with | [] => false | _ :: ss => matchPath (Pat.Skip :: ps) ss)
  | Pat.Star :: ps, _ :: ss =>
    matchPath ps ss
  | Pat.Lit l :: ps, s :: ss =>
    if l.contains '*' then
      -- Simple implementation of interior '*'
      let parts := l.splitOn "*"
      match parts with
      | [pre, post] => s.startsWith pre && s.endsWith post && matchPath ps ss
      | _ => l == s && matchPath ps ss
    else
      l == s && matchPath ps ss
  | _, _ => false

public def filePatternMatch (pat : String) (path : String) : Bool :=
  matchPath (parse pat) (path.splitOn "/")

end Shake.Development

end
