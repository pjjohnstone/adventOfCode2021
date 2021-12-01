open System.IO

let rec didItIncreaseRec (measurements: int list) previous (increases: int list) =
  match measurements with
  | [] -> []
  | _ ->
    match previous with
    | None -> didItIncreaseRec measurements.Tail (Some(measurements.Head)) []
    | Some p ->
      if measurements.Head > p
      then didItIncreaseRec measurements.Tail (Some(measurements.Head)) increases@[measurements.Head]
      else didItIncreaseRec measurements.Tail (Some(measurements.Head)) increases

let didItIncrease measurements =
  didItIncreaseRec measurements None []

let lines = File.ReadAllLines("input.txt")

lines
|> Array.toList
|> List.map System.Int32.Parse
|> didItIncrease
|> fun l -> printfn "It increased %i times" l.Length