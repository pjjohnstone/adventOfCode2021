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

let getLength points =
  let ((x1,y1),(x2,y2)) = points
  let result = if x1 = x2 then y1 - y2 else x1 - x2
  if result < 0 then (result * -1) else result

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

let countIntersects lines =
  lines
  |> List.map Seq.toList
  |> List.concat
  |> List.countBy (fun p -> p)
  |> List.filter (fun (_,c) -> c > 1)
  |> List.distinct
  |> List.length

let coordinates = List.map coordinatesFromLine input
let straightLines = List.filter straightLine coordinates
let drawnLines =
  straightLines
  |> List.map (fun l -> drawLine (getLength l) l)

printfn "The coordinates are %A" coordinates
printfn "The straight lines are: %A" straightLines
for drawnLine in drawnLines do
  printfn "Drawn line was: %A" drawnLine
printfn "Number of intersects is: %i" (countIntersects drawnLines)
