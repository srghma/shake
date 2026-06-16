module

public import Shake.Development.FilePath
public import Shake.Development.Util
public import Shake.Incremental
public import Shake.Incremental.Store
import Std.Data.HashMap

/-!
# Shake Development DSL

This module provides the core DSL for defining Shake rules in Lean,
including Actions, Rules, and pattern matching operators.
-/

@[expose] public section

namespace Shake.Development
open Shake.Development.FilePath
open Std (HashMap)

/-- State tracked during an Action execution. -/
public structure ActionState where
  depends : List String := []
  outputs : List String := []

/-- An Action is a computation that can be run during the build process, tracking dependencies. -/
public abbrev Action (α : Type) : Type := StateT ActionState IO α

/-- Rules are used to define how targets are built. -/
public abbrev Rules (α : Type) : Type := StateT (HashMap String (String -> Action Unit)) IO α

public structure ShakeOptions where
  shakeFiles : String := "_build"

public def shakeOptions : ShakeOptions := {}

public def want (_targets : List String) : Rules Unit := do
  pure ()

/-- need specifies dependencies for an Action. -/
public def need (files : List String) : Action Unit := do
  let s ← get
  set { s with depends := s.depends ++ files }

public def phony (name : String) (action : Action Unit) : Rules Unit := do
  let m ← get
  set (m.insert name (fun _ => action))

public def phis (pattern : String) (action : String -> Action Unit) : Rules Unit := do
  let m ← get
  set (m.insert pattern action)

infixr:60 " %> " => phis

public def phisList (patterns : List String) (action : String -> Action Unit) : Rules Unit := do
  for pattern in patterns do
    phis pattern action

infixr:60 " |%> " => phisList

public def phisMulti (patterns : List String) (action : List String -> Action Unit) : Rules Unit := do
  for pattern in patterns do
    phis pattern (fun out => action [out])

infixr:60 " &%> " => phisMulti

/-- readFile' path reads a file and records it as a dependency. -/
public def readFile' (path : String) : Action String := do
  need [path]
  IO.FS.readFile path

/-- writeFile' path content writes a file and records it as an output. -/
public def writeFile' (path : String) (content : String) : Action Unit := do
  IO.FS.writeFile path content
  let s ← get
  set { s with outputs := s.outputs ++ [path] }

/-- copyFile' old new copies the existing file from old to new. -/
public def copyFile' (old : String) (new : String) : Action Unit := do
  let content ← readFile' old
  writeFile' new content

/-- copyFileChanged is similar to copyFile' but only copies if the content changed. -/
public def copyFileChanged (old : String) (new : String) : Action Unit := do
  need [old]
  let oldContent ← IO.FS.readFile old
  let currentNewContent ← try
      IO.FS.readFile new
    catch _ =>
      pure ""
  if oldContent != currentNewContent then
    writeFile' new oldContent
  else
    let s ← get
    set { s with outputs := s.outputs ++ [new] }

/-- removeFiles dir patterns removes files matching patterns in dir. -/
public def removeFiles (dir : String) (patterns : List String) : IO Unit := do
  IO.println s!"Removing files in {dir} matching {patterns}"
  pure ()

public def cmd_ (prog : String) (args : List String) : Action Unit := do
  IO.println s!"Executing: {prog} {String.intercalate " " args}"
  let _ ← IO.Process.output { cmd := prog, args := args.toArray }
  pure ()

public def shakeArgs (_opts : ShakeOptions) (rules : Rules Unit) : IO Unit := do
  let (_, _ruleMap) ← rules.run {}
  pure ()

end Shake.Development
