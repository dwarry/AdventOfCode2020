module Day7

open System.IO
open System.Text.RegularExpressions

open QuickGraph
open QuickGraph.Algorithms.Search
open QuickGraph.Algorithms.Observers
open QuickGraph.Algorithms.ShortestPath


let readFile path = 
  let graph = AdjacencyGraph<string, Edge<string>>()
  let edgeCosts = System.Collections.Generic.Dictionary<Edge<string>, int>()
  let parseSourceNode src = Regex.Match(src, @"^(\w+ \w+)").Groups.[1].Value
  let parseDestNode dest = 
    let m = Regex.Match(dest, @"(\d+) (\w+ \w+) bag")
    if m.Success then
        let count = m.Groups.[1].Value
        let colour = m.Groups.[2].Value
        Some (colour, (int count))
    else
        None

  let addEdgeAndWeight src t = 
    match t with
    | Some weightedEdge -> 
        let e = Edge(src, (fst weightedEdge))
        graph.AddEdge(e) |> ignore
        edgeCosts.Add(e, (snd weightedEdge))
    | None -> ()

  File.ReadAllLines(path)
  |> Array.iter (fun (line: string) -> 
                   let parts = line.Split([| " contain "|], System.StringSplitOptions.None)
                   let src = parts.[0] |> parseSourceNode
                   graph.AddVertex(src) |> ignore
                   parts.[1].Split(',') |> Array.iter (parseDestNode >> (addEdgeAndWeight src)))
                   
  graph, edgeCosts
   
                   
let findShinyGoldContainers (g: AdjacencyGraph<string, Edge<string>>) = 
  let containers colour = 
    g.Edges 
    |> Seq.filter (fun e -> e.Target = colour) 
    |> Seq.map (fun e -> e.Source)

  let rec findAllContainers (res: Set<string>) (colour: string) = 
     printf "%s\n" colour
    
     containers colour 
     |> Seq.fold (fun r c ->
                      findAllContainers (Set.add c r) c) res
       
  findAllContainers (Set.empty<string>) "shiny gold"
      
let part2 (g: AdjacencyGraph<string, Edge<string>>)
          (costs: System.Collections.Generic.IDictionary<Edge<string>, int>) =
    let rec processVertex colour = 
        g.OutEdges(colour)
        |> Seq.fold (fun acc next -> 
                         let cost = costs.[next] 
                         acc + cost + (cost * (processVertex next.Target))) 0
    processVertex "shiny gold"

