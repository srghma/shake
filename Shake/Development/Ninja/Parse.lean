/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development.Ninja.Env
public import Shake.Development.Ninja.Type
public import Shake.Development.Ninja.Lexer
public import Shake.Internal.Core.Extra

/-!
# Ninja Parser implementation for Lean 4.
-/

@[expose] public section

namespace Shake.Development.Ninja

open Shake.Internal.Core

public partial def withBinds : List Lexeme → List (Lexeme × List (Str × Expr))
  | [] => []
  | x :: xs =>
    let (as, bs) := f xs
    (x, as) :: withBinds bs
where
  f : List Lexeme → List (Str × Expr) × List Lexeme
    | Lexeme.LexBind a b :: rest =>
      let (as, bs) := f rest
      ((a, b) :: as, bs)
    | xs => ([], xs)

public def splitDeps : List Str → List Str × List Str × List Str
  | [] => ([], [], [])
  | x :: xs =>
    let (a, b, c) := splitDeps xs
    if x == "|" then ([], a ++ b, c)
    else if x == "||" then ([], b, a ++ c)
    else (x :: a, b, c)

public def getDepth (env : Env Str Str) (xs : List (Str × Expr)) : IO Int :=
  match xs.find? (·.1 == "depth") with
  | none => pure 1
  | some (_, x) => do
    let s ← askExpr env x
    match s.toInt? with
    | some i => pure i.toNat
    | none => throw (IO.userError s!"Ninja parsing, could not parse depth field in pool, got: {s}")

mutual
  public partial def parse (file : System.FilePath) (env : Env Str Str) : IO Ninja :=
    parseFile file env newNinja

  public partial def parseFile (file : System.FilePath) (env : Env Str Str) (ninja : Ninja) :
      IO Ninja := do
    let content ← IO.FS.readFile file
    let lexes := lexer content
    let stmts := withBinds lexes
    let mut currentNinja := { ninja with sources := file :: ninja.sources }
    for stmt in stmts do
      currentNinja ← applyStmt env currentNinja stmt
    return currentNinja

  public partial def applyStmt (env : Env Str Str) (ninja : Ninja)
      (stmt : Lexeme × List (Str × Expr)) : IO Ninja :=
    match stmt.1 with
    | Lexeme.LexBuild outputs rule deps => do
      let outputs' ← outputs.mapM (askExpr env)
      let deps' ← deps.mapM (askExpr env)
      let binds' ← stmt.2.mapM (fun (a, b) => do return (a, ← askExpr env b))
      let (normal, implicit, orderOnly) := splitDeps deps'
      let build := Build.mk rule env normal implicit orderOnly binds'
      if rule == "phony" then
        let newPhonys := outputs'.map (fun x => (x, normal ++ implicit ++ orderOnly))
        return { ninja with phonys := newPhonys ++ ninja.phonys }
      else if outputs'.length == 1 then
        return { ninja with singles := (outputs'.head!, build) :: ninja.singles }
      else
        return { ninja with multiples := (outputs', build) :: ninja.multiples }
    | Lexeme.LexRule name =>
      return { ninja with rules := (name, Rule.mk stmt.2) :: ninja.rules }
    | Lexeme.LexDefault xs => do
      let xs' ← xs.mapM (askExpr env)
      return { ninja with defaults := xs' ++ ninja.defaults }
    | Lexeme.LexPool name => do
      let depth ← getDepth env stmt.2
      return { ninja with pools := (name, depth) :: ninja.pools }
    | Lexeme.LexInclude expr => do
      let file ← askExpr env expr
      parseFile (System.FilePath.mk file) env ninja
    | Lexeme.LexSubninja expr => do
      let file ← askExpr env expr
      let e ← scopeEnv env
      parseFile (System.FilePath.mk file) e ninja
    | Lexeme.LexDefine a b => do
      addBind env a b
      return ninja
    | Lexeme.LexBind a _ =>
      throw (IO.userError s!"Ninja parsing, unexpected binding defining {a}")
end

end Shake.Development.Ninja

end
