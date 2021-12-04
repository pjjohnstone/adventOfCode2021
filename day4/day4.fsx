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
  line.Split ','
  |> Array.toList
  |> List.map (fun v -> { Value = (System.Int32.Parse(v)); Called = false })

let rec getBoard lines board =
  match lines with
  | [] -> board
  | _ ->
    let newRow = { Spaces = getRow lines.Head }
    getBoard lines.Tail { board with Rows = board.Rows@[newRow] }
