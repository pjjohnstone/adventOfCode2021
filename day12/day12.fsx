open System.IO
open System

type Node = {
  Name: string;
  Big: bool;
  Connected: string list
}

type Route = {
  Start: Node;
  End: Node;
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

let findConnections (lines: string[]) node nodes =
  lines
  |> Array.filter (fun l -> l.Contains(node.Name))
  |> Array.map (fun l -> l.Split('-'))
  |> Array.concat
  |> Array.distinct
  |> Array.filter (fun l -> not (l = node.Name))
  |> Array.toList
  |> fun c -> { node with Connected = c }

let findAllConnections lines nodes =
  seq {
    for node in nodes do yield findConnections lines node nodes
  } |> Seq.toList

let rec findRoutesRec routes remainingNodes (nodes: Node list) =
  match remainingNodes with
  | [] -> routes
  | _ ->
    let newRoutes =
      seq {
        for connection in remainingNodes.Head.Connected do
        yield {
          Start = remainingNodes.Head;
          End = (nodes |> List.find (fun n -> n.Name = connection));
        }
      } |> Seq.toList
    findRoutesRec (List.append routes newRoutes) remainingNodes.Tail nodes

let findRoutes nodes =
  findRoutesRec [] nodes nodes

nodesFromLines input
|> findAllConnections input
|> findRoutes
|> printfn "%A"