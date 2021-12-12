open System.IO

let example = File.ReadAllLines("example.txt")
let input = File.ReadAllLines("input.txt")

type ChunkDelimiter =
  | Opening of char
  | Closing of char
  | Invalid of char

type NavLogLine =
  | Valid of char[] * int
  | Corrupt of char[] * int

let (|OpeningChar|ClosingChar|InvalidChar|) char =
  match char with
  | '(' -> OpeningChar(char)
  | '[' -> OpeningChar(char)
  | '{' -> OpeningChar(char)
  | '<' -> OpeningChar(char)
  | ')' -> ClosingChar(char)
  | ']' -> ClosingChar(char)
  | '}' -> ClosingChar(char)
  | '>' -> ClosingChar(char)
  | _ -> InvalidChar(char)

let nextCharLegal lastOpening char =
  match char with
  | OpeningChar(c) -> Opening(c)
  | ClosingChar(c) ->
    match lastOpening with
      | '(' ->
        match c with
        | ')' -> Closing(c)
        | _ -> Invalid(c)
      | '[' ->
        match c with
        | ']' -> Closing(c)
        | _ -> Invalid(c)
      | '{' ->
        match c with
        | '}' -> Closing(c)
        | _ -> Invalid(c)
      | '<' ->
        match c with
        | '>' -> Closing(c)
        | _ -> Invalid(c)
      | _ -> Invalid(c)
  | InvalidChar(i) -> Invalid(i)

let rec parseNavigationLine (openings: char list) index (navLine: char array) =
  let validNextChar = nextCharLegal openings.Head navLine.[index]
  match ((Array.length navLine) - index) with
  | 1 ->
    match validNextChar with
    | Invalid(_) -> Corrupt(navLine,index)
    | Closing(_) -> Valid(navLine,index)
    | Opening(_) -> Corrupt(navLine,index)
  | _ ->
    match validNextChar with
    | Invalid(_) -> Corrupt(navLine,index)
    | Closing(_) -> parseNavigationLine (openings.Tail) (index + 1) navLine
    | Opening(o) -> parseNavigationLine (o::openings) (index + 1) navLine

let parseNavigationLogLines lines =
  lines
  |> Array.toList
  |> List.map (fun s -> Seq.toArray s)
  |> List.map (fun l ->
    match l.[0] with
    | OpeningChar(o) -> parseNavigationLine [o] 0 l
    | _ -> Corrupt(l,0))

let rec countScore total (parsedLines: NavLogLine list) =
  match parsedLines with
  | [] -> total
  | _ ->
    match parsedLines.Head with
    | Valid(_) -> countScore total parsedLines.Tail
    | Corrupt(c,i) ->
      let points =
        match c.[i] with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0
      countScore (total + points) parsedLines.Tail

printfn "Penalty score: %i" (countScore 0 (parseNavigationLogLines input))
