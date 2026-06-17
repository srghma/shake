/-
Copyright (c) 2024 Jules. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jules
-/
module

public import Shake.Development
public import Shake.Development.FilePath
public import Shake.Development.FilePattern
public import Shake.Internal.Core.Binary
public import Shake.Development.Ninja.Env
public import Shake.Development.Ninja.Type
public import Shake.Development.Ninja.Lexer
public import Shake.Paths
public import Shake.Internal.Core.Intern

@[expose] public section

def testFilePath : IO Unit := do
  let path := "dir/subdir/file.txt"
  assert! (Shake.Development.FilePath.dropDirectory1 path == "subdir/file.txt")
  assert! (Shake.Development.FilePath.takeDirectory1 path == "dir")
  assert! (Shake.Development.FilePath.dropExtension path == "dir/subdir/file")
  IO.println "FilePath tests passed."

def testFilePattern : IO Unit := do
  assert! (Shake.Development.filePatternMatch "src/**/*.lean" "src/Shake/Incremental.lean")
  assert! (Shake.Development.filePatternMatch "src/*.lean" "src/Main.lean")
  assert! (!Shake.Development.filePatternMatch "src/*.lean" "src/Shake/Incremental.lean")
  IO.println "FilePattern tests passed."

def testSerialization : IO Unit := do
  let data := "test-data"
  let bytes := Shake.Internal.Core.Binary.put data
  match Shake.Internal.Core.Binary.get (α := String) bytes with
  | some (s, _) => assert! (s == data)
  | none => panic! "Serialization failed"

  let listData : List UInt64 := [1, 2, 3, 4]
  let listBytes := Shake.Internal.Core.Binary.put listData
  match Shake.Internal.Core.Binary.get (α := List UInt64) listBytes with
  | some (l, _) => assert! (l == listData)
  | none => panic! "List serialization failed"

  IO.println "Binary serialization tests passed."

def testNinjaEnv : IO Unit := do
  let env : Shake.Development.Ninja.Env String String ← Shake.Development.Ninja.newEnv
  Shake.Development.Ninja.addEnv env "var" "val"
  let res ← Shake.Development.Ninja.askEnv env "var"
  assert! (res == some "val")

  let sEnv ← Shake.Development.Ninja.scopeEnv env
  Shake.Development.Ninja.addEnv sEnv "var2" "val2"
  assert! ((← Shake.Development.Ninja.askEnv sEnv "var") == some "val")
  assert! ((← Shake.Development.Ninja.askEnv sEnv "var2") == some "val2")
  IO.println "Ninja Env tests passed."

def testNinjaType : IO Unit := do
  let env ← Shake.Development.Ninja.newEnv
  Shake.Development.Ninja.addEnv env "x" "X"
  let expr := Shake.Development.Ninja.Expr.Exprs [Shake.Development.Ninja.Expr.Lit "a", Shake.Development.Ninja.Expr.Var "x", Shake.Development.Ninja.Expr.Lit "b"]
  let res ← Shake.Development.Ninja.askExpr env expr
  assert! (res == "aXb")
  IO.println "Ninja Type tests passed."

def testNinjaLexer : IO Unit := do
  let s := "var = val\nbuild out: rule dep\n"
  let res := Shake.Development.Ninja.lexer s
  match res with
  | [Shake.Development.Ninja.Lexeme.LexDefine "var" (Shake.Development.Ninja.Expr.Lit "val"),
     Shake.Development.Ninja.Lexeme.LexBuild [Shake.Development.Ninja.Expr.Lit "out"] "rule" [Shake.Development.Ninja.Expr.Lit "dep"]] =>
    IO.println "Ninja Lexer tests passed."
  | _ =>
    IO.eprintln s!"Ninja Lexer tests failed: {repr res}"
    panic! "Lexer failed"

def testPaths : IO Unit := do
  assert! (Shake.Paths.version == [0, 1, 0])
  IO.println "Paths tests passed."

def testIntern : IO Unit := do
  let intern := Shake.Internal.Core.emptyIntern (α := String)
  let (intern, id1) := Shake.Internal.Core.addIntern "a" intern
  let (intern, id2) := Shake.Internal.Core.addIntern "b" intern
  assert! (Shake.Internal.Core.lookupIntern "a" intern == some id1)
  assert! (Shake.Internal.Core.lookupIntern "b" intern == some id2)
  assert! (id1 != id2)
  IO.println "Intern tests passed."

def main : IO Unit := do
  IO.println "Running Shake tests..."
  testFilePath
  testFilePattern
  testSerialization
  testNinjaEnv
  testNinjaType
  testNinjaLexer
  testPaths
  testIntern
  IO.println "All tests passed successfully!"
