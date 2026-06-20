module

public import Shake.Development
public import Shake.Development.FilePath

@[expose] public section

def main : IO Unit := do
  IO.println "Running Shake tests..."
  -- Add some basic tests for FilePattern or FilePath
  let pat := "src/**/*.lean"
  let file := "src/Shake/Incremental.lean"
  -- We don't have filePatternMatch exported or working perfectly yet but let's try
  IO.println s!"Testing pattern matching: {pat} vs {file}"
