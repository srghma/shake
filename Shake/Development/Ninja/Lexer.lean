/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development.Ninja.Type
public import Std.Internal.Parsec

/-!
# Ninja Lexer implementation for Lean 4 using Parsec.
-/

@[expose] public section

namespace Shake.Development.Ninja

open Std.Internal.Parsec
open Std.Internal.Parsec.String

public inductive Lexeme where
  | LexBind (var : Str) (val : Expr)
  | LexBuild (outputs : List Expr) (rule : Str) (deps : List Expr)
  | LexInclude (file : Expr)
  | LexSubninja (file : Expr)
  | LexRule (name : Str)
  | LexPool (name : Str)
  | LexDefault (files : List Expr)
  | LexDefine (var : Str) (val : Expr)
  deriving Repr, BEq

namespace Lexer

public def isVar (c : Char) : Bool :=
  c == '-' || c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

public def isVarDot (c : Char) : Bool :=
  c == '.' || isVar c

public partial def skipMany' (p : Parser α) : Parser Unit :=
  (p *> skipMany' p) <|> pure ()

public def dropSpace : Parser Unit := skipMany' (pchar ' ')

public def jumpCont : Parser Unit := do
  let c? ← peek?
  if c? == some '$' then
    let _ ← pchar '$'
    match ← peek? with
    | some '\n' => let _ ← pchar '\n'; dropSpace
    | some '\r' =>
      let _ ← pchar '\r'
      if (← peek?) == some '\n' then let _ ← pchar '\n'; dropSpace
      else pure ()
    | _ => pure ()
  else pure ()

public def isSpecial (stopColon stopSpace : Bool) (c : Char) : Bool :=
  match stopColon, stopSpace with
  | true, true => c == ':' || c == ' ' || c == '$' || c == '\r' || c == '\n' || c == '\x00'
  | true, false => c == ':' || c == '$' || c == '\r' || c == '\n' || c == '\x00'
  | false, true => c == ' ' || c == '$' || c == '\r' || c == '\n' || c == '\x00'
  | false, false => c == '$' || c == '\r' || c == '\n' || c == '\x00'

public partial def collectLit (stopColon stopSpace : Bool) (acc : String) : Parser (String × Bool) := do
  match ← peek? with
  | none => return (acc, true)
  | some c =>
    if isSpecial stopColon stopSpace c then return (acc, false)
    else
      let _ ← any
      collectLit stopColon stopSpace (acc.push c)

public partial def collectVarDot (acc : String) : Parser String := do
  match ← peek? with
  | some c =>
    if isVarDot c then
      let _ ← any
      collectVarDot (acc.push c)
    else return acc
  | none => return acc

public partial def collectVar (acc : String) : Parser String := do
  match ← peek? with
  | some c =>
    if isVar c then
      let _ ← any
      collectVar (acc.push c)
    else return acc
  | none => return acc

public def dropLast (s : String) : String :=
  if s.isEmpty then "" else s.toRawSubstring.dropRight 1 |>.toString

mutual
  public partial def lexxExprF (stopColon stopSpace : Bool) : Parser (List Expr) := do
    let (lit, _) ← collectLit stopColon stopSpace ""
    if lit == "" then
      lexxExprG stopColon stopSpace
    else
      let exprs ← lexxExprG stopColon stopSpace
      return Expr.Lit lit :: exprs

  public partial def lexxExprG (stopColon stopSpace : Bool) : Parser (List Expr) := do
    match ← peek? with
    | some '$' =>
      let _ ← pchar '$'
      match ← peek? with
      | some '$' => let _ ← pchar '$'; return Expr.Lit "$" :: (← lexxExprF stopColon stopSpace)
      | some ' ' => let _ ← pchar ' '; return Expr.Lit " " :: (← lexxExprF stopColon stopSpace)
      | some ':' => let _ ← pchar ':'; return Expr.Lit ":" :: (← lexxExprF stopColon stopSpace)
      | some '\n' => let _ ← pchar '\n'; dropSpace; lexxExprF stopColon stopSpace
      | some '\r' =>
        let _ ← pchar '\r'
        if (← peek?) == some '\n' then let _ ← pchar '\n'; dropSpace; lexxExprF stopColon stopSpace
        else lexxExprF stopColon stopSpace
      | some '{' =>
        let _ ← pchar '{'
        let var ← collectVarDot ""
        let _ ← pchar '}'
        return Expr.Var var :: (← lexxExprF stopColon stopSpace)
      | _ =>
        let var ← collectVar ""
        if var == "" then return []
        else return Expr.Var var :: (← lexxExprF stopColon stopSpace)
    | _ => return []

  public partial def lexxExpr (stopColon stopSpace : Bool) : Parser Expr := do
    let acc ← lexxExprF stopColon stopSpace
    return match acc with
      | [x] => x
      | xs => Expr.Exprs xs

  public partial def lexxExprs (stopColon : Bool) : Parser (List Expr) := do
    let a ← lexxExpr stopColon true
    match ← peek? with
    | some ' ' =>
      let _ ← pchar ' '
      dropSpace
      let as ← lexxExprs stopColon
      return a :: as
    | some ':' =>
      if stopColon then
        let _ ← pchar ':'
        return [a]
      else
        return [a]
    | _ => return [a]

  public partial def splitLineCR : Parser Str := do
    let s ← manyChars (satisfy (· != '\n'))
    if (← peek?) == some '\n' then skip
    return if s.endsWith "\r" then dropLast s else s

  public partial def splitLineCont : Parser Str := do
    let line ← splitLineCR
    if line.endsWith "$" then
      let line' := dropLast line
      dropSpace
      let rest ← splitLineCont
      return line' ++ rest
    else
      return line

  public partial def lexBuild : Parser (List Lexeme) := do
    let outputs ← lexxExprs true
    dropSpace
    jumpCont
    let rule ← collectVarDot ""
    dropSpace
    let deps ← lexxExprs false
    let rest ← lexerLoop
    return Lexeme.LexBuild outputs rule deps :: rest

  public partial def lexRule : Parser (List Lexeme) := do
    dropSpace
    let name ← splitLineCont
    let rest ← lexerLoop
    return Lexeme.LexRule name :: rest

  public partial def lexDefault : Parser (List Lexeme) := do
    dropSpace
    let files ← lexxExprs false
    let rest ← lexerLoop
    return Lexeme.LexDefault files :: rest

  public partial def lexPool : Parser (List Lexeme) := do
    dropSpace
    let name ← splitLineCont
    let rest ← lexerLoop
    return Lexeme.LexPool name :: rest

  public partial def lexInclude : Parser (List Lexeme) := do
    dropSpace
    let file ← lexxExpr false false
    let rest ← lexerLoop
    return Lexeme.LexInclude file :: rest

  public partial def lexSubninja : Parser (List Lexeme) := do
    dropSpace
    let file ← lexxExpr false false
    let rest ← lexerLoop
    return Lexeme.LexSubninja file :: rest

  public partial def lexDefine : Parser (List Lexeme) := do
    let var ← collectVarDot ""
    if var == "" then
      if (← peek?) == none then return []
      else let _ ← any; lexerLoop
    else
      dropSpace
      jumpCont
      if (← peek?) == some '=' then
        let _ ← pchar '='
        dropSpace
        jumpCont
        let val ← lexxExpr false false
        let rest ← lexerLoop
        return Lexeme.LexDefine var val :: rest
      else
        lexerLoop

  public partial def lexerLoop : Parser (List Lexeme) := do
    match ← peek? with
    | none => return []
    | some '\r' | some '\n' => let _ ← any; lexerLoop
    | some ' ' =>
      let _ ← pchar ' '
      dropSpace
      match ← peek? with
      | some '\n' | some '\r' | some '#' | none => lexerLoop
      | _ =>
        let var ← collectVarDot ""
        dropSpace
        jumpCont
        if (← peek?) == some '=' then
          let _ ← pchar '='
          dropSpace
          jumpCont
          let val ← lexxExpr false false
          let rest ← lexerLoop
          return Lexeme.LexBind var val :: rest
        else
          lexerLoop
    | some '#' =>
      let _ ← pchar '#'
      let _ ← manyChars (satisfy (· != '\n'))
      lexerLoop
    | some 'b' => (attempt (skipString "build ") *> lexBuild) <|> lexDefine
    | some 'r' => (attempt (skipString "rule ") *> lexRule) <|> lexDefine
    | some 'd' => (attempt (skipString "default ") *> lexDefault) <|> lexDefine
    | some 'p' => (attempt (skipString "pool ") *> lexPool) <|> lexDefine
    | some 'i' => (attempt (skipString "include ") *> lexInclude) <|> lexDefine
    | some 's' => (attempt (skipString "subninja ") *> lexSubninja) <|> lexDefine
    | _ => lexDefine
end

end Lexer

public def lexer (s : String) : List Lexeme :=
  match (Lexer.lexerLoop ⟨s, s.startPos⟩) with
  | .success _ res => res
  | .error _ _ => []

end Shake.Development.Ninja

end
