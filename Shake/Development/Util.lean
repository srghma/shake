import Shake.Development.FilePath
namespace Shake.Development.Util
def parseMakefile (contents : String) : List (String × List String) :=
  let lines := contents.splitOn "\n"
  let filteredLines := lines.filter (fun l => !l.trimAscii.isEmpty)
  filteredLines.filterMap fun line =>
    match line.splitOn ":" with
    | [target, deps] =>
      let target := target.trimAscii.toString
      let deps := deps.trimAscii.toString.splitOn " " |>.map (fun s => s.trimAscii.toString) |>.filter (fun s => !s.isEmpty)
      some (target, deps)
    | _ => none
end Shake.Development.Util
