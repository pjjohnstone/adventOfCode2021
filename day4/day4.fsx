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

getBoards example.Tail
