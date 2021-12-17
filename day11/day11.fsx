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

let tryFindIndexInGrid y x (grid: 'a[,]) =
  if (y < 0 || y > Array2D.length1 grid - 1) || (x < 0 || x > Array2D.length2 grid - 1)
  then None
  else Some(y,x)

let neighbours y x grid =
  [
    tryFindIndexInGrid (y - 1) (x - 1) grid;
    tryFindIndexInGrid (y - 1) x grid;
    tryFindIndexInGrid (y - 1) (x + 1) grid;
    tryFindIndexInGrid y (x - 1) grid;
    tryFindIndexInGrid y (x + 1) grid;
    tryFindIndexInGrid (y + 1) (x - 1) grid;
    tryFindIndexInGrid (y + 1) x grid;
    tryFindIndexInGrid (y + 1) (x + 1) grid;
  ]

// let charge grid =
//   grid
//   |> Array2D.map (fun v -> v + 1)

let charge grid =
  for y = 0 to Array2D.length1 grid - 1 do
    for x = 0 to Array2D.length2 grid - 1 do
      Array2D.set grid y x (grid[y,x] + 1)

let mutable flashes = 0

let flash y x grid =
  neighbours y x grid
  |> List.iter (fun n ->
    match n with
    | None -> ()
    | Some (y2,x2) ->
      Array2D.set grid y2 x2 (grid.[y2,x2] + 1))
  Array2D.set grid y x 0

let rec step grid =
  for y = 0 to Array2D.length1 grid - 1 do
    for x = 0 to Array2D.length2 grid - 1 do
      if grid.[y,x] > 8 then
        flash y x grid
        flashes <- flashes + 1
        step grid

let rec countFlashesRec maxSteps currentStep grid =
  match ((maxSteps + 1) - currentStep) with
  | 0 -> ()
  | _ ->
    printfn "Step %i" currentStep
    printfn "%A" grid
    printfn "Flashes: %i" flashes
    step grid
    charge grid
    countFlashesRec maxSteps (currentStep + 1) grid

let countFlashes maxSteps grid =
  countFlashesRec maxSteps 0 grid

let grid = inputLinesToIntArrays example
flashes <- 0
countFlashes 10 grid