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

type Number = {
  Code: string
  Digit: int
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
  let one = inputs |> List.find (fun s -> s.Length = 2)
  let seven = inputs |> List.find (fun s -> s.Length = 3)
  let four = inputs |> List.find (fun s -> s.Length = 4)
  let eight = inputs |> List.find (fun s -> s.Length = 7)
  [
    { Code = one; Digit = 1 }
    { Code = seven; Digit = 7 }
    { Code = four; Digit = 4 }
    { Code = eight; Digit = 8 }
  ]

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
  let lowerLeftAndBottom = eight |> Array.except (Array.concat [four;top])
  let zero = inputs |> List.find (fun i -> (i.Length = 6) && (searchStringforChars (Array.append seven lowerLeftAndBottom) i))
  let middle = upperLeftAndMiddle |> Array.except zero
  let upperLeft = upperLeftAndMiddle |> Array.except middle
  let three = inputs |> List.find (fun i -> (i.Length = 5) && (searchStringforChars (Array.append seven middle) i))
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
  [
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = 0 }
    { Code = [|mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = 1}
    { Code = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = 2 }
    { Code = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerRight; mappings.Bottom|] |> Array.sort |> System.String; Digit = 3}
    { Code = [|mappings.UpperLeft; mappings.Middle; mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = 4}
    { Code = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = 5}
    { Code = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = 6}
    { Code = [|mappings.Top; mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = 7}
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = 8}
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = 9}
  ]

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

let one = knownDigits |> List.filter (fun n -> n.Digit = 1) |> List.exactlyOne |> fun n -> n.Code |> Seq.toArray
let seven = knownDigits |> List.filter (fun n -> n.Digit = 7) |> List.exactlyOne |> fun n -> n.Code |> Seq.toArray
let four = knownDigits |> List.filter (fun n -> n.Digit = 4) |> List.exactlyOne |> fun n -> n.Code |> Seq.toArray
let eight = knownDigits |> List.filter (fun n -> n.Digit = 8) |> List.exactlyOne |> fun n -> n.Code |> Seq.toArray

let mapped = resolveMappings  one seven four eight io.Inputs
let code = mappingsToNumbers mapped