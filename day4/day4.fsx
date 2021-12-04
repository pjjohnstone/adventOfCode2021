open System.IO

let lines = File.ReadAllLines("input.txt") |> Array.toList
let example = File.ReadAllLines("example.txt") |> Array.toList

let numbers (lines: string list) =
  lines.Head.Split ','
  |> Array.toList
  |> List.map (fun n -> System.Int32.Parse(n))

type Space = {
  Value: int;
  Called: bool;
}

type Row = {
  Spaces: Space list;
}

type Board = {
  Rows: Row list;
}

let getRow (line: string) =
  line.Split ' '
  |> Array.toList
  |> List.filter (fun v -> v.Length > 0)
  |> List.map (fun v -> { Value = (System.Int32.Parse(v)); Called = false })

let rec getBoardRec lines board =
  match lines with
  | [] -> board
  | _ ->
    let newRow = { Spaces = getRow lines.Head }
    getBoardRec lines.Tail { board with Rows = board.Rows@[newRow] }

let getBoard lines =
  getBoardRec lines { Rows = [] }

let rec getBoardsRec lines (boards: Board list) =
  match lines with
  | [] -> boards
  | _ ->
    match lines.Head with
    | "" -> getBoardsRec lines.Tail boards
    | _ ->
      let newBoard = getBoard lines.[0..4]
      let newBoards = newBoard::boards
      let newLines = lines.[5..]
      getBoardsRec newLines newBoards

let getBoards lines =
  getBoardsRec lines []

let isFullRow row =
  if row.Spaces |> List.forall (fun s -> s.Called) then true else false

let columnAsRow (board: Board) column =
  board.Rows
  |> List.map (fun c -> c.Spaces.[column])
  |> fun r -> { Spaces = r }

let rec columnsAsRowsRec inBoard column (outBoard: Board) =
  match column with
  | 5 -> outBoard
  | _ ->
    let newRow = columnAsRow inBoard column
    columnsAsRowsRec inBoard (column + 1) ({ outBoard with Rows = outBoard.Rows@[newRow]})

let columnsAsRows board =
  columnsAsRowsRec board 0 { Rows = [] }

let isWinningBoard board =
  if
    board.Rows |> List.exists isFullRow ||
      board |> columnsAsRows |> fun b -> b.Rows |> List.exists isFullRow
  then true
  else false

let callSpace space =
  { space with Called = true }

let numberCalled space number =
  if space.Value = number then true else false

let tryCallValueInRow row value =
  row.Spaces
  |> List.map (fun s -> if s.Value = value then callSpace s else s)
  |> fun s -> { row with Spaces = s }

let tryCallValueInBoard board value =
  board.Rows
  |> List.map (fun r -> tryCallValueInRow r value)
  |> fun r -> { board with Rows = r }

let callNumber boards number =
  boards |> List.map (fun b -> tryCallValueInBoard b number)

let rec callNumbersRec boards numbers lastNumber =
  match numbers with
  | [] -> (boards, lastNumber)
  | _ ->
    let winner = boards |> List.exists isWinningBoard
    match winner with
    | true -> (boards, lastNumber)
    | false ->
      let newBoards = callNumber boards numbers.Head
      callNumbersRec newBoards numbers.Tail numbers.Head

let calculateScore winningNumber board  =
  let spaceValue =
    board.Rows
    |> List.map (fun r ->
      r.Spaces
      |> List.map (fun s -> if not s.Called then s.Value else 0)
      |> List.sum)
    |> List.sum
  spaceValue * winningNumber

let boards = getBoards lines.Tail

callNumbersRec boards (numbers lines) 0
|> fun (b,n) ->
  b
  |> List.filter isWinningBoard
  |> List.exactlyOne
  |> calculateScore n
  |> printfn "The winning score was: %i"
