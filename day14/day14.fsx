open System.IO
open System.Collections.Generic

let example = File.ReadAllLines(@"example.txt")
let input = File.ReadAllLines(@"input.txt")

let parseInput lines =
  let index = Array.findIndex (fun l -> l = "") lines
  let (polymer, rules) = Array.splitAt index lines
  ((polymer |> Array.exactlyOne |> Seq.toList), (Array.tail rules |> Array.toList))

let ruleToTuple (rule: string) =
  (rule.[0..1], Seq.last rule)

let rulesToTuples rules =
  rules
  |> List.map ruleToTuple

let newElement (rules: list<string * char>) left right =
  let pairString = $"%c{left}%c{right}"
  rules |> List.find (fun (p,_) -> p = pairString) |> fun (_,i) -> i

let mutable counts = new Dictionary<char, int>()

let incrementKey key =
  match counts.ContainsKey key with
  | true -> counts[key] <- counts[key] + 1
  | false -> counts.Add(key, 1)

let rec expand rules left right depth =
  match (depth = 0) with
  | true -> incrementKey left
  | false ->
    expand rules left (newElement rules left right) (depth - 1)
    expand rules (newElement rules left right) right (depth - 1)

let expandPolymer rules polymer depth =
  polymer
  |> Seq.pairwise
  |> Seq.iter (fun (l,r) -> expand rules l r depth)

let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let summarizeCounts (counts: Dictionary<char,int>) =
  toMap counts
  |> Map.toList
  |> List.map (fun (_,c) -> c)
  |> fun l -> printfn "polymer value is: %i" (List.max l - List.min l)

let (polymer,ruleStrings) = parseInput example
let rules = rulesToTuples ruleStrings

expandPolymer rules polymer 10
incrementKey (List.last polymer)
printfn "%A" counts
summarizeCounts counts