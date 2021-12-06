let example = "3,4,3,1,2"

let input = "1,3,1,5,5,1,1,1,5,1,1,1,3,1,1,4,3,1,1,2,2,4,2,1,3,3,2,4,4,4,1,3,1,1,4,3,1,5,5,1,1,3,4,2,1,5,3,4,5,5,2,5,5,1,5,5,2,1,5,1,1,2,1,1,1,4,4,1,3,3,1,5,4,4,3,4,3,3,1,1,3,4,1,5,5,2,5,2,2,4,1,2,5,2,1,2,5,4,1,1,1,1,1,4,1,1,3,1,5,2,5,1,3,1,5,3,3,2,2,1,5,1,1,1,2,1,1,2,1,1,2,1,5,3,5,2,5,2,2,2,1,1,1,5,5,2,2,1,1,3,4,1,1,3,1,3,5,1,4,1,4,1,3,1,4,1,1,1,1,2,1,4,5,4,5,5,2,1,3,1,4,2,5,1,1,3,5,2,1,2,2,5,1,2,2,4,5,2,1,1,1,1,2,2,3,1,5,5,5,3,2,4,2,4,1,5,3,1,4,4,2,4,2,2,4,4,4,4,1,3,4,3,2,1,3,5,3,1,5,5,4,1,5,1,2,4,2,5,4,1,3,3,1,4,1,3,3,3,1,3,1,1,1,1,4,1,2,3,1,3,3,5,2,3,1,1,1,5,5,4,1,2,3,1,3,1,1,4,1,3,2,2,1,1,1,3,4,3,1,3"

let maxDays = 18

type Cohort = {
  Age: int;
  Number: int;
}

let getFish (string: string) =
  string.Split ','
  |> Array.toList
  |> List.map System.Int32.Parse
  |> List.countBy (fun f -> f)
  |> List.map (fun (a,n) -> { Age = a; Number = n })

let spawnFish fish = seq {
  for f in fish do
    if f.Age = 0 then yield { Age = 8; Number = f.Number }
}

let countFish fish =
  fish
  |> List.map (fun f -> f.Number)
  |> List.sum

let rec combineCohorts combined cohorts  =
  match cohorts with
  | [] -> combined
  | _ ->
    let (matches,notMatches) = cohorts |> List.partition (fun c -> c.Age = cohorts.Head.Age)
    let collapsedMatches =
      matches |> countFish |> fun n -> { Age = matches.Head.Age; Number = n }
    combineCohorts (collapsedMatches::combined) notMatches

let updateSchool fish =
  let newFish = spawnFish fish |> Seq.toList
  fish
  |> List.map (fun f -> if f.Age = 0 then { f with Age = 7 } else f)
  |> List.map (fun f -> { f with Age = (f.Age - 1)})
  |> List.append newFish
  |> combineCohorts []

let rec runSimulation fish (stopDay: int) day =
  match (day > stopDay) with
  | true -> fish
  | _ ->
    let newSchool = updateSchool fish
    printfn "After %i days: %A" day fish
    printfn "After %i days there are %i fish" day (countFish fish)
    runSimulation newSchool stopDay (day+1)

runSimulation (getFish example) maxDays 0