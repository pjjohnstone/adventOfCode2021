open System.IO

let example = File.ReadAllLines("example.txt")

type ChunkDelimiter =
  | Opening of char
  | Closing of char
  | Invalid of char

type NavLogLine =
  | Valid of char[]
  | Corrupt of char[]

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

let rec parseNavigationLine (openings: char list) (navLine: char array) =
  let validNextChar = nextCharLegal openings.Head navLine.[0]
  match (Array.length navLine) with
  | 1 ->
    match validNextChar with
    | Invalid(_) -> Corrupt(navLine)
    | Closing(_) -> Valid(navLine)
    | Opening(_) -> Corrupt(navLine)
  | _ ->
    match validNextChar with
    | Invalid(_) -> Corrupt(navLine)
    | Closing(_) -> parseNavigationLine (openings.Tail) navLine.[1..]
    | Opening(o) -> parseNavigationLine (o::openings) navLine.[1..]

let parseNavigationLogLines lines =
  lines
  |> Array.toList
  |> List.map (fun s -> Seq.toArray s)
  |> List.map (fun l ->
    match l.[0] with
    | OpeningChar(o) -> parseNavigationLine [o] l
    | _ -> Corrupt(l))

let rec countScore total (parsedLines: NavLogLine list) =
  match parsedLines with
  | [] -> total
  | _ ->
    match parsedLines.Head with
    | Valid(_) -> countScore total parsedLines.Tail
    | Corrupt(c) ->
      let points =
        match c.[0] with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0
      countScore (total + points) parsedLines.Tail

printfn "Penalty score: %i" (countScore 0 (parseNavigationLogLines example))
