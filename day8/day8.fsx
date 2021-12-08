open System.IO

let example = File.ReadAllLines("example.txt")
let input = File.ReadAllLines("input.txt")

type SignalPattern = {
  Inputs: string list;
  Outputs: string list;
}

let chopUpString (string: string) =
  string.Split ' '
  |> Array.toList
  |> List.filter (fun s -> s.Length > 0)

let getInputsAndOutputs (strings: string[]) =
  let patterns = strings |> Array.toList
  seq {
    for pattern in patterns do
    let (inputs,outputs) =
      pattern.Split '|'
      |> Array.toList
      |> fun l -> (l.Head, List.last l)
      |> fun (i,o) -> (chopUpString i, chopUpString o)
    yield { Inputs = inputs; Outputs = outputs }
  } |> Seq.toList

let knownOutput (output: string) =
  match output.Length with
  | 2 -> true
  | 3 -> true
  | 4 -> true
  | 7 -> true
  | _ -> false

let countKnownOutputs (patterns: SignalPattern list) =
  seq {
    for pattern in patterns do
    yield pattern.Outputs |> List.filter knownOutput
  } |> List.concat |> List.length

let signalPatterns = getInputsAndOutputs input

printfn "There were: %i" (countKnownOutputs signalPatterns)