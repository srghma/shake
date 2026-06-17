/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development.Ninja.Env

/-!
# Ninja types and structure definitions.
-/

@[expose] public section

namespace Shake.Development.Ninja

public abbrev Str := String
public abbrev FileStr := String

public inductive Expr where
  | Exprs (xs : List Expr)
  | Lit (x : Str)
  | Var (x : Str)
  deriving BEq, Repr

public partial def askExpr (e : Env Str Str) : Expr → IO Str
  | Expr.Exprs xs => do
    let ss ← xs.mapM (askExpr e)
    pure (String.join ss)
  | Expr.Lit x => pure x
  | Expr.Var x => do
    let val ← askEnv e x
    pure (val.getD "")

public def askVar (e : Env Str Str) (x : Str) : IO Str := do
  let val ← askEnv e x
  pure (val.getD "")

public def addBind (e : Env Str Str) (k : Str) (v : Expr) : IO Unit := do
  let val ← askExpr e v
  addEnv e k val

public def addBinds (e : Env Str Str) (bs : List (Str × Expr)) : IO Unit := do
  for (k, v) in bs do
    addBind e k v

public structure Rule where
  ruleBind : List (Str × Expr)

public structure Build where
  ruleName : Str
  env : Env Str Str
  depsNormal : List FileStr
  depsImplicit : List FileStr
  depsOrderOnly : List FileStr
  buildBind : List (Str × Str)

public structure Ninja where
  sources : List System.FilePath
  rules : List (Str × Rule)
  singles : List (FileStr × Build)
  multiples : List (List FileStr × Build)
  phonys : List (Str × List FileStr)
  defaults : List FileStr
  pools : List (Str × Int)

public def newNinja : Ninja :=
  ⟨[], [], [], [], [], [], []⟩

end Shake.Development.Ninja

end
