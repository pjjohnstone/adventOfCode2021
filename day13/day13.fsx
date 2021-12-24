open System.IO

let example = File.ReadAllLines("example.txt")

type Coordinate = {
  X: int;
  Y: int;
}

type Fold = {
  Axis: char;
  Position: int;
}

let getCoords lines =
  let index = lines |> Array.findIndex (fun l -> l = "")
  lines
  |> Array.splitAt index
  |> fun (c,_) ->
    c
    |> Array.map (fun x -> x.Split(','))
    |> Array.map (fun a -> { X = System.Int32.Parse a.[0]; Y = System.Int32.Parse a.[1] })
    |> Array.toList

let parseFoldInstruction line =
  line
  |> Seq.toList
  |> List.splitAt 12
  |> fun (d,p) ->
    { Axis = List.last d; Position = System.Char.GetNumericValue (List.last p) |> int }

let getFolds lines =
  let index = lines |> Array.findIndex (fun l -> l = "")
  lines
  |> Array.skip (index + 1)
  |> Array.map parseFoldInstruction

let coords = getCoords example
let folds = getFolds example