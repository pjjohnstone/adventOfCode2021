open System.IO

let example = File.ReadAllLines(@"day14\example.txt")
let input = File.ReadAllLines(@"day14\input.txt")

let parseInput lines =
  let index = Array.findIndex (fun l -> l = "") lines
  let (polymer, rules) = Array.splitAt index lines
  ((polymer |> Array.exactlyOne |> Seq.toList), (Array.tail rules |> Array.toList))

let ruleToTuple (rule: string) =
  (rule.[0..1], Seq.last rule)

let rulesToTuples rules =
  rules
  |> List.map ruleToTuple

let polymerInsert rules pair =
  let (left,right) = pair
  let pairString = $"%c{left}%c{right}"
  let insert = rules |> List.find (fun (p,_) -> p = pairString) |> fun (_,i) -> i
  $"%c{left}%c{insert}"

let step rules (polymer: char seq) =
  polymer
  |> Seq.pairwise
  |> Seq.map (fun pair -> polymerInsert rules pair)
  |> String.concat ""
  |> fun s -> $"%s{s}%c{(Seq.last polymer)}"

let rec stepsRec rules maxCount (polymer: char seq) count =
  match (count > maxCount) with
  | true -> polymer
  | false ->
    printfn "After step %i: %i" count (Seq.length polymer)
    stepsRec rules maxCount (step rules polymer) (count + 1)

let steps rules maxCount (polymer: char seq) =
  stepsRec rules maxCount polymer 1

let scorePolymer polymer =
  let counts = polymer |> List.countBy id
  let highestFreq = counts |> List.map (fun (_,c) -> c) |> List.max
  let lowestFreq = counts |> List.map (fun (_,c) -> c) |> List.min
  printfn "polymer value is: %i" (highestFreq - lowestFreq)

let (polymer,ruleStrings) = parseInput input
let rules = rulesToTuples ruleStrings
steps rules 40 polymer |> Seq.toList |> scorePolymer