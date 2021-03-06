open System.IO

let example = File.ReadAllLines(@"day13\example.txt")
let input = File.ReadAllLines(@"day13\input.txt")

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
    { Axis = List.last d; Position = (List.skip 1 p) |> List.toArray |> System.String.Concat |> int }

let getFolds lines =
  let index = lines |> Array.findIndex (fun l -> l = "")
  lines
  |> Array.skip (index + 1)
  |> Array.map parseFoldInstruction
  |> Array.toList

let foldY foldPosition point =
  let distance = point.Y - foldPosition
  if point.Y >= foldPosition then { point with Y = (point.Y - distance * 2) } else point

let foldX foldPosition point =
  let distance = point.X - foldPosition
  if point.X >= foldPosition then { point with X = (point.X - distance * 2) } else point

let fold coords fold =
  match fold.Axis with
  | 'y' -> List.map (foldY fold.Position) coords
  | _ -> List.map (foldX fold.Position) coords

let drawPattern coords =
  let lengthY = coords |> List.maxBy (fun c -> c.Y) |> fun c -> c.Y
  let lengthX = coords |> List.maxBy (fun c -> c.X) |> fun c -> c.X
  let grid = Array2D.create (lengthY + 1) (lengthX + 1) '.'
  coords
  |> List.iter (fun c -> grid.[c.Y,c.X] <- 'O')
  printfn "%A" grid

let countHoles coords =
  coords |> List.distinct |> List.length |> printfn "There are %i holes"

let allFolds folds coords =
  folds |> List.map (fold coords)

let coords = getCoords input
let folds = getFolds input
countHoles coords
let newCoords = allFolds folds coords
countHoles newCoords