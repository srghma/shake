import Std.Data.HashMap
import Std.Data.HashSet
import Init.System.FilePath

open System (FilePath)

def findHaskellImports (content : String) : List String :=
  let lines := content.splitOn "\n"
  lines.filter (·.trimAsciiStart.toString.startsWith "import ") |>.filterMap fun line =>
    let parts := line.trimAscii.toString.splitOn " "
    match parts with
    | "import" :: "qualified" :: name :: _ => some (name.replace "." "/")
    | "import" :: name :: _ => some (name.replace "." "/")
    | _ => none

partial def walkDir (dir : FilePath) : IO (Array FilePath) := do
  let entries ← System.FilePath.readDir dir
  let mut result := #[]
  for entry in entries do
    let full := dir / entry.fileName
    if ← full.isDir then
      result := result ++ (← walkDir full)
    else if full.extension == some "hs" then
      result := result.push full
  return result

def main : IO Unit := do
  let hsFiles ← walkDir "src"
  let mut deps : Std.HashMap String (Std.HashSet String) := {}
  let mut allNames : Array String := #[]

  for f in hsFiles do
    let rel := (f.toString.drop 4).replace ".hs" ""
    allNames := allNames.push rel
    let content ← IO.FS.readFile f
    let imps := findHaskellImports content
    let mut hsImps : Std.HashSet String := {}
    for imp in imps do
       hsImps := hsImps.insert imp
    deps := deps.insert rel hsImps

  -- Simple topological sort (Kahn's)
  let mut inDegree : Std.HashMap String Nat := {}
  for name in allNames do inDegree := inDegree.insert name 0
  for (src, targets) in deps.toList do
    for target in targets.toList do
      if inDegree.contains target then
        inDegree := inDegree.insert src (inDegree.getD src 0 + 1)

  let mut queue := allNames.filter (fun name => inDegree.getD name 0 == 0) |>.toList
  let mut sorted : List String := []

  while !queue.isEmpty do
    match queue with
    | [] => break
    | cur :: rest =>
      queue := rest
      sorted := cur :: sorted
      for (src, targets) in deps.toList do
        if targets.contains cur then
          if inDegree.contains src then
            let d := inDegree.getD src 0 - 1
            inDegree := inDegree.insert src d
            if d == 0 then queue := src :: queue

  let mut md := "# Haskell to Lean Porting Status\n\n"
  md := md ++ "Files are topologically sorted by dependency order (independent first).\n\n"

  for name in sorted.reverse do
    let leanPath := if name == "Development/Shake" then "Development.lean" else s!"{name}.lean"
    let ported := if (← System.FilePath.pathExists s!"Shake/{leanPath}") then "[x]" else "[ ]"
    md := md ++ s!"- {ported} src/{name}.hs\n"

  IO.FS.writeFile "PORTING_STATUS.md" md
  IO.println "Generated PORTING_STATUS.md"
