open System.IO

let example = File.ReadAllLines("example.txt")

let inputLinesToIntArrays (lines: string[]) =
  lines
  |> Array.map (fun l ->
    l
    |> Seq.toArray
    |> Array.map (fun c -> System.Char.GetNumericValue c |> int))
  |> array2D

let tryFindItemInGrid y x (grid: 'a[,]) =
  if (y < 0 || y > Array2D.length1 grid - 1) || (x < 0 || x > Array2D.length2 grid - 1)
  then None
  else Some(grid.[y,x])

let neighbours y x grid =
  (
    tryFindItemInGrid (y - 1) (x - 1) grid,
    tryFindItemInGrid (y - 1) x grid,
    tryFindItemInGrid (y - 1) (x + 1) grid,
    tryFindItemInGrid y (x - 1) grid,
    tryFindItemInGrid y (x + 1) grid,
    tryFindItemInGrid (y + 1) (x - 1) grid,
    tryFindItemInGrid (y + 1) x grid,
    tryFindItemInGrid (y + 1) (x + 1) grid
  )

let grid = inputLinesToIntArrays example
printfn "Neighbours of pos %i,%i: %A" 4 4 (neighbours 4 4 grid)
printfn "Neighbours of pos %i,%i: %A" 0 0 (neighbours 0 0 grid)
printfn "Neighbours of pos %i,%i: %A" 9 9 (neighbours 9 9 grid)