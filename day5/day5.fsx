open System.IO

let example = File.ReadAllLines("example.txt") |> Array.toList
let input = File.ReadAllLines("input.txt") |> Array.toList

let coordinatesFromLine (line: string) =
  line.Split ' '
  |> Array.toList
  |> fun l -> (l.Head, (List.last l))
  |> fun (x,y) ->
    let xCoords = x.Split ',' |> Array.toList |> List.map System.Int32.Parse
    let yCoords = y.Split ',' |> Array.toList |> List.map System.Int32.Parse
    ((xCoords.Head,(List.last xCoords)),(yCoords.Head,(List.last yCoords)))

let sameX points =
  let ((x1,_),(x2,_)) = points
  if x1 = x2 then true else false

let sameY points =
  let ((_,y1),(_,y2)) = points
  if y1 = y2 then true else false

let straightLine points =
  if sameX points || sameY points then true else false

let getDistance (a,b) =
  let length = a - b
  if length < 0 then (length * -1) else length

let diagonalLine points =
  let ((x1,y1),(x2,y2)) = points
  let xDistance = getDistance (x1,x2)
  let yDistance = getDistance (y1,y2)
  if xDistance = yDistance then true else false

let getLength points =
  let ((x1,y1),(x2,y2)) = points
  if x1 = x2 then getDistance (y1,y2) else getDistance (x1,x2)

let drawLine length points = seq {
  let ((x1,y1),(x2,y2)) = points
  if sameX points then
    if y2 > y1 then
      for i = y1 to (y1 + length) do yield (x1,i)
    else
      for i = y2 to (y2 + length) do yield (x1,i)
  else
    if x2 > x1 then
      for i = x1 to (x1 + length) do
        yield (i,y1)
    else
      for i = x2 to (x2 + length) do yield (i,y1)
}

let positiveTranslation x1 x2 =
  if (x2 - x1) >= 0 then true else false

let (|XposYpos|XposYneg|XnegYpos|XnegYneg|) ((x1,x2),(y1,y2)) =
  if (positiveTranslation x1 x2) && (positiveTranslation y1 y2) then XposYpos
  else if (positiveTranslation x1 x2) && not (positiveTranslation y1 y2) then XposYneg
  else if not (positiveTranslation x1 x2) && (positiveTranslation y1 y2) then XnegYpos
  else XnegYneg

let drawDiagonalLine length points = seq {
  let ((x1,y1),(_,_)) = points
  match points with
  | XposYpos -> for i = 0 to length do yield ((x1+i),(y1+i))
  | XposYneg -> for i = 0 to length do yield ((x1+i),(y1-i))
  | XnegYpos -> for i = 0 to length do yield ((x1-i),(y1+i))
  | XnegYneg -> for i = 0 to length do yield ((x1-i),(y1-i))
}

let countIntersects lines =
  lines
  |> List.map Seq.toList
  |> List.concat
  |> List.countBy (fun p -> p)
  |> List.filter (fun (_,c) -> c > 1)
  |> List.distinct
  |> List.length

let coordinates = List.map coordinatesFromLine example
let straightLines = List.filter straightLine coordinates
let diagonalLines = List.filter diagonalLine coordinates
let drawnStraightLines =
  straightLines
  |> List.map (fun l -> drawLine (getLength l) l)

let drawnDiagonalLines =
  diagonalLines
  |> List.map (fun d -> drawDiagonalLine (getLength d) d)

printfn "The coordinates are %A" coordinates
printfn "The straight lines are: %A" straightLines
printfn "The diagonal lines are: %A" diagonalLines
for drawnLine in drawnStraightLines do
  printfn "Drawn straight line was: %A" drawnLine
for drawnLine in drawnDiagonalLines do
  printfn "Drawn diagonal line was: %A" drawnLine
printfn "Number of intersects is: %i" (countIntersects (drawnStraightLines@drawnDiagonalLines))
drawnDiagonalLines
|> List.iter (fun s ->
  s
  |> Seq.iter (fun c ->
    printfn "%A" c))
