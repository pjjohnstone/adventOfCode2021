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

let charge grid =
  for y = 0 to Array2D.length1 grid - 1 do
    for x = 0 to Array2D.length2 grid - 1 do
      Array2D.set grid y x (grid[y,x] + 1)

let flash y x grid =
  neighbours y x grid
  |> List.iter (fun n ->
    match n with
    | None -> ()
    | Some (y2,x2) ->
      Array2D.set grid y2 x2 (grid.[y2,x2] + 1))

let zeroFlashers flashers grid =
  flashers
  |> List.iter (fun (y,x) -> Array2D.set grid y x 0)

let rec step y x flashers grid =
  match (y = Array2D.length1 grid) with
  | true -> flashers
  | false ->
    match (x = Array2D.length2 grid) with
    | true -> step (y + 1) 0 flashers grid
    | false ->
      match (grid.[y,x] > 8) with
      | false -> step y (x + 1) flashers grid
      | true ->
        flash y x grid
        let newFlashers = (y,x)::flashers
        step y (x + 1) newFlashers grid

let startStepping grid =
  step 0 0 [] grid

let rec countFlashesRec maxSteps currentStep countFlashes grid =
  match ((maxSteps + 1) - currentStep) with
  | 0 -> ()
  | _ ->
    let flashes = startStepping grid
    charge grid
    zeroFlashers flashes grid
    let totalFlashes = countFlashes + (flashes |> List.length)
    printfn "After step %i:\n%A" currentStep grid
    printfn "Flashes: %i" totalFlashes
    countFlashesRec maxSteps (currentStep + 1) totalFlashes grid

let countFlashes maxSteps grid =
  countFlashesRec maxSteps 1 grid

let grid = inputLinesToIntArrays example
countFlashes 3 0 grid