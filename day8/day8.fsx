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
  Digit: char
}

let chopUpString (string: string) =
  string.Split ' '
  |> Array.toList
  |> List.filter (fun s -> s.Length > 0)
  |> List.map (fun s ->
    s
    |> Seq.toArray
    |> Array.sort
    |> System.String)

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
  (one,seven,four,eight)

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
  printfn "Top: %A" top
  let upperLeftAndMiddle = four |> Array.except one
  printfn "ulam: %A" upperLeftAndMiddle
  let lowerLeftAndBottom = eight |> Array.except (Array.concat [four;top])
  printfn "llab: %A" lowerLeftAndBottom
  let zero = inputs |> List.find (fun i -> (i.Length = 6) && (searchStringforChars (Array.append seven lowerLeftAndBottom) i))
  printfn "Zero: %A" zero
  let middle = upperLeftAndMiddle |> Array.except zero
  printfn "middle: %A" middle
  let upperLeft = upperLeftAndMiddle |> Array.except middle
  printfn "ul: %A" upperLeft
  let three = inputs |> List.find (fun i -> (i.Length = 5) && (searchStringforChars (Array.append seven middle) i))
  printfn "three: %A" three
  let lowerLeft = lowerLeftAndBottom |> Array.except three
  printfn "ll: %A" lowerLeft
  let bottom = lowerLeftAndBottom |> Array.except lowerLeft
  printfn "bottom: %A" bottom
  let six = inputs |> List.find (fun i -> (i.Length = 6) && (searchStringforChars lowerLeft i) && (searchStringforChars middle i))
  printfn "six: %A" six
  let upperRight = one |> Array.except six
  printfn "ur: %A" upperRight
  let lowerRight = one |> Array.except upperRight
  printfn "lr: %A" lowerRight
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
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = '0' }
    { Code = [|mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = '1' }
    { Code = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = '2' }
    { Code = [|mappings.Top; mappings.UpperRight; mappings.Middle; mappings.LowerRight; mappings.Bottom|] |> Array.sort |> System.String; Digit = '3' }
    { Code = [|mappings.UpperLeft; mappings.Middle; mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = '4' }
    { Code = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = '5' }
    { Code = [|mappings.Top; mappings.LowerRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = '6' }
    { Code = [|mappings.Top; mappings.UpperRight; mappings.LowerRight|] |> Array.sort |> System.String; Digit = '7' }
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom; mappings.LowerLeft|] |> Array.sort |> System.String; Digit = '8' }
    { Code = [|mappings.Top; mappings.LowerRight; mappings.UpperRight; mappings.Middle; mappings.UpperLeft; mappings.Bottom|] |> Array.sort |> System.String; Digit = '9' }
  ]

let countKnownOutputs (patterns: SignalPattern list) =
  seq {
    for pattern in patterns do
    yield pattern.Outputs |> List.filter knownOutput
  } |> List.concat |> List.length


let outputsToChars numbers outputs =
  seq {
    for output in outputs do
    let numberOption = numbers |> List.tryFind (fun n -> n.Code = output)
    printfn "NumberOption: %A" numberOption
    match numberOption with
    | Some n ->
      yield n.Digit
    | _ -> ()
  }

let totalFromSignals signals =
  seq {
    for signal in signals do
    printfn "Signal: %A" signal
    let (one,seven,four,eight) = getKnownNumbers signal.Inputs
    printfn "One: %A" one
    printfn "Seven: %A" seven
    printfn "Four: %A" four
    printfn "Eight: %A" eight
    let mappings = resolveMappings one seven four eight signal.Inputs
    printfn "Mappings: %A" mappings
    let cipher = mappingsToNumbers mappings
    printfn "Cipher: %A" cipher
    yield outputsToChars cipher signal.Outputs |> Seq.toArray |> System.String |> System.Int32.Parse
  } //|> Seq.sum

let signalPatterns = getInputsAndOutputs example

printfn "There were %i known outputs" (countKnownOutputs signalPatterns)

let shortExample = [|"fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"|]

let signals = getInputsAndOutputs shortExample

printfn "Total of all signals is: %A" (totalFromSignals signals)