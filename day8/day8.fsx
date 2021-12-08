open System.IO

let example = File.ReadAllLines("example.txt")

type SignalPattern = {
  Inputs: string list;
  Outputs: string list;
}

let getInputsAndOutputs (strings: string[]) =
  let patterns = strings |> Array.toList
  seq {
    for pattern in patterns do
    let (inputs,outputs) =
      pattern.Split '|'
      |> Array.toList
      |> fun l -> (l.Head, List.last l)
      |> fun (i,o) -> (i.Split ' ' |> Array.toList, o.Split ' ' |> Array.toList)
    yield { Inputs = inputs; Outputs = outputs }
  }

printfn "%A" (getInputsAndOutputs example)