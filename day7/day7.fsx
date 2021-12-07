open System.IO

let example = "16,1,2,0,4,2,7,1,2,14"
let input = File.ReadAllLines("input.txt") |> Array.exactlyOne

let getCrabs (string: string) =
  string.Split ',' |> Array.toList |> List.map System.Int32.Parse

let costToPosition position crab =
  let fuelCost = position - crab
  if fuelCost < 0 then (fuelCost * -1) else fuelCost

let totalCostToPosition position crabs =
  crabs |> List.map (fun c -> costToPosition c position) |> List.sum

let furthestCrab crabs =
  crabs |> List.sort |> List.last

let findCheapestPosition crabs =
  let costs = seq {
    for i = 0 to (furthestCrab crabs) do yield (totalCostToPosition i crabs)
  }
  costs
  |> Seq.toList
  |> List.sort
  |> List.head
  |> printfn "The cheapest move costs %i fuel"

findCheapestPosition (getCrabs input)