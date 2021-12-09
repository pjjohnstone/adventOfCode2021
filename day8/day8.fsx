open System.IO

let example = File.ReadAllLines("example.txt")
let input = File.ReadAllLines("input.txt")

type SignalPattern = {
  Inputs: string list;
  Outputs: string list;
}

type DisplayMapping = {
  Top: char
  UpperLeft: char
  LowerLeft: char
  Middle: char
  Bottom: char
  UpperRight: char
  LowerRight: char
}

type Numbers = {
  One: char[]
  Two: char[]
  Three: char[]
  Four: char[]
  Five: char[]
  Six: char[]
  Seven: char[]
  Eight: char[]
  Nine: char[]
}

let chopUpString (string: string) =
  string.Split ' '
  |> Array.toList
  |> List.filter (fun s -> s.Length > 0)

let getInputsAndOutputs (strings: string[]) =
  let patterns = strings |> Array.toList
  seq {
    for pattern in patterns do
    let (inputs,outputs) =
      pattern.Split '|'
      |> Array.toList
      |> fun l -> (l.Head, List.last l)
      |> fun (i,o) -> (chopUpString i, chopUpString o)
    yield { Inputs = inputs; Outputs = outputs }
  } |> Seq.toList

let knownOutput (output: string) =
  match output.Length with
  | 2 -> true
  | 3 -> true
  | 4 -> true
  | 7 -> true
  | _ -> false

let getKnownNumbers (inputs: string list) =
  let one = inputs |> List.find (fun s -> s.Length = 2) |> Seq.toArray
  let seven = inputs |> List.find (fun s -> s.Length = 3) |> Seq.toArray
  let four = inputs |> List.find (fun s -> s.Length = 4) |> Seq.toArray
  let eight = inputs |> List.find (fun s -> s.Length = 7) |> Seq.toArray
  {
    One = one
    Two = [|'x'|]
    Three = [|'x'|]
    Four = four
    Five = [|'x'|]
    Six = [|'x'|]
    Seven = seven
    Eight = eight
    Nine = [|'x'|]
  }

let rec containsAllElements list2 list1 =
  match list1 with
  | [] -> false
  | _ ->
    match list2 with
    | [] -> true
    | h::t ->
      if not (List.contains h list1) then false
      else containsAllElements t list1

let searchStringListForChars chars stringList =
  stringList
  |> List.find (fun s ->
    s
    |> Seq.toList
    |> containsAllElements (Array.toList chars))
  |> Seq.toArray

let searchStringforChars chars string =
  string |> Seq.toList |> containsAllElements (Array.toList chars)

let resolveMappings one seven four eight (inputs: string list) =
  let top = seven |> Array.except one
  let upperLeftAndMiddle = four |> Array.except one
  printfn "UpperLeftAndMiddle: %A" upperLeftAndMiddle
  let lowerLeftAndBottom = eight |> Array.except (Array.concat [four;top])
  printfn "LowerLeftAndBottom: %A" lowerLeftAndBottom
  let zero = inputs |> List.find (fun i -> (i.Length = 6) && (searchStringforChars (Array.append seven lowerLeftAndBottom) i))
  printfn "Zero: %A" zero
  let middle = upperLeftAndMiddle |> Array.except zero
  printfn "Middle: %A" middle
  let upperLeft = upperLeftAndMiddle |> Array.except middle
  printfn "UpperLeft: %A" upperLeft
  let three = inputs |> List.find (fun i -> (i.Length = 5) && (searchStringforChars (Array.append seven middle) i))
  printfn "Three: %A" three
  let lowerLeft = lowerLeftAndBottom |> Array.except three
  let bottom = lowerLeftAndBottom |> Array.except lowerLeft
  let six = inputs |> List.find (fun i -> (i.Length = 6) && (searchStringforChars lowerLeft i))
  let upperRight = one |> Array.except six
  let lowerRight = one |> Array.except upperRight
  {
    Top = top |> Array.exactlyOne;
    UpperLeft = upperLeft |> Array.exactlyOne;
    LowerLeft = lowerLeft |> Array.exactlyOne;
    Middle = middle |> Array.exactlyOne;
    Bottom = bottom |> Array.exactlyOne;
    UpperRight = upperRight |> Array.exactlyOne;
    LowerRight = lowerRight |> Array.exactlyOne;
  }

let mappingsToNumbers mappings =
  {
    One = [|mappings.UpperRight; mappings.LowerRight|]
    Two = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerLeft; mappings.Bottom|]
    Three = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerRight; mappings.Bottom|]
    Four = [|mappings.UpperLeft; mappings.Middle; mappings.UpperRight; mappings.LowerRight|]
    Five = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|]
    Six = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|]
    Seven = [|mappings.Top; mappings.UpperRight; mappings.LowerRight|]
    Eight = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|]
    Nine = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|]
  }

let countKnownOutputs (patterns: SignalPattern list) =
  seq {
    for pattern in patterns do
    yield pattern.Outputs |> List.filter knownOutput
  } |> List.concat |> List.length

let signalPatterns = getInputsAndOutputs example

printfn "There were %i known outputs" (countKnownOutputs signalPatterns)

let shortExample = [|"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"|]

let io = getInputsAndOutputs shortExample |> List.exactlyOne

let knownDigits = getKnownNumbers io.Inputs

let mapped = resolveMappings knownDigits.One knownDigits.Seven knownDigits.Four knownDigits.Eight io.Inputs
let code = mappingsToNumbers mapped