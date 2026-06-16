module

public import Shake.Development
public import Shake.Development.FilePath
public import Shake.Development.FilePattern
public import Shake.Internal.Core.Binary

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

def main : IO Unit := do
  IO.println "Running Shake tests..."
  testFilePath
  testFilePattern
  testSerialization
  IO.println "All tests passed successfully!"
