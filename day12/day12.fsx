open System.IO
open System

type Node = {
  Name: string;
  Big: bool;
  Connected: string list
}

let input = File.ReadAllLines("example1.txt")

let isUpper (str : string) =
  let rec strIter isUpper arr =
    match arr with
    | [] -> isUpper
    | _ ->
      match Char.IsLower(arr.Head) with
      | true -> strIter false []
      | false -> strIter true arr.Tail
  strIter true (Array.toList <| str.ToCharArray())

let nodesFromLines (lines: string[]) =
  seq {
    for line in lines do yield line.Split('-')
  }
  |> Seq.concat
  |> Seq.distinct
  |> Seq.toList
  |> List.map (fun n ->
    {
      Name = n;
      Big = (isUpper n);
      Connected = [];
    })

let findConnections (lines: string[]) node =
  lines
  |> Array.filter (fun l -> l.Contains(node.Name))
  |> Array.map (fun l -> l.Split('-'))
  |> Array.concat
  |> Array.distinct
  |> Array.filter (fun l -> not (l = node.Name))
  |> Array.toList
  |> fun c -> { node with Connected = c }

let nodes = nodesFromLines input
let connectedNodes = nodes |> List.map (fun n -> findConnections input n)