open System.IO

let example = File.ReadAllLines(@"day14\example.txt")

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
  printfn "Left: %c Right: %c" left right
  printfn "String version: %s" pairString
  printfn "Inserting: %c" insert
  printfn $"%c{left}%c{insert}"
  $"%c{left}%c{insert}"

let step rules polymer =
  polymer
  |> List.pairwise
  |> List.map (fun pair -> polymerInsert rules pair)
  |> String.concat ""
  |> fun s -> $"%s{s}%c{(List.last polymer)}"

let (polymer,rules) = parseInput example
let ruleTuples = rulesToTuples rules
let pairs = polymer |> List.pairwise
let step1 = step ruleTuples polymer
let step2 = step ruleTuples (step1 |> Seq.toList)