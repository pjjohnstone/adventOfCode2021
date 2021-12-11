open System.IO

let example = File.ReadAllLines("example.txt")

let inputLinesToIntArrays (lines: string[]) =
  lines
  |> Array.map (fun l ->
    l
    |> Seq.toArray
    |> Array.map (fun c -> System.Char.GetNumericValue c |> int))

let up y x (grid: int[,]) =
  if y = 0 then grid.[y,x] else grid.[y - 1, x]

let down y x (grid: int[,]) =
  if y = Array2D.length1 grid - 1 then grid.[y,x] else grid.[y + 1, x]

let left y x (grid: int[,]) =
  if x = 0 then grid.[y,x] else grid.[y, x - 1]

let right y x (grid: int[,]) =
  if x = Array2D.length2 grid - 1 then grid.[y,x] else grid.[y, x + 1]

let neighbourValues y x (grid: int[,]) =
  ((up y x grid),(down y x grid),(left y x grid),(right y x grid))

let isLowPoint y x grid =
  let (up,down,left,right) = neighbourValues y x grid
  let values = [(grid.[y,x]); up; down; left; right]
  if
    (List.sort values).[0] = grid.[y,x]
  then true
  else false

let arrays = inputLinesToIntArrays example
let grid = array2D arrays

isLowPoint 4 0 grid