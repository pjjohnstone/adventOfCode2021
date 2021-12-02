open System.IO

let lines = File.ReadAllLines("input.txt") |> Array.toList

let down (a,h,d) change = (a+change,h,d)

let up (a,h,d) change = (a-change,h,d)

let forward (a,h,d) change = (a,h+change,d+(a*change))

let doCommand (a,h,d) (line: string) =
  let (command, param) =
    let l =line.Split ' '
    (l[0], (System.Int32.Parse l[1]))
  match command with
  | "down" -> down (a,h,d) param
  | "up" -> up (a,h,d) param
  | "forward" -> forward (a,h,d) param
  | _ -> (a,h,d)

let rec determinePositionRec (lines: string list) (a,h,d) =
  match lines with
  | [] -> (a,h,d)
  | _ ->
    let (newA, newH, newD) = doCommand (a,h,d) lines.Head
    determinePositionRec lines.Tail (newA, newH, newD)

let determinePosition lines =
  determinePositionRec lines (0,0,0)

lines
|> determinePosition
|> fun (_,h,d) -> h*d
|> printfn "Product of position is: %i"
