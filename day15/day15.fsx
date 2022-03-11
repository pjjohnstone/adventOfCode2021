open System.IO

let example = File.ReadAllLines(@"day15\example.txt")

let inputLinesToIntArrays (lines: string[]) =
  lines
  |> Array.map (fun l ->
    l
    |> Seq.toArray
    |> Array.map (fun c -> System.Char.GetNumericValue c |> int))
  |> array2D

let tryFindIndexInGrid y x (grid: 'a[,]) =
  if (y < 0 || y > Array2D.length1 grid - 1) || (x < 0 || x > Array2D.length2 grid - 1)
  then None
  else Some(y,x)

let neighbours y x grid =
  [
    tryFindIndexInGrid (y - 1) x grid;
    tryFindIndexInGrid (y + 1) x grid;
    tryFindIndexInGrid y (x - 1) grid;
    tryFindIndexInGrid y (x + 1) grid;
  ]

let isVisited visited (y,x) =
  visited
  |> List.exists (fun (vY,vX) -> (vY = y && vX = x))

let isDestination grid (y,x) =
  if (y = Array2D.length1 grid - 1) && (x = Array2D.length2 grid - 1)
  then true
  else false

let validMoves grid visited (y,x) =
  neighbours y x grid
  |> List.choose id
  |> List.filter (fun (y,x) -> not(isVisited visited (y,x)))

let grid = inputLinesToIntArrays example
