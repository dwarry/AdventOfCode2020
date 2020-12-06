module Day6

open System.IO

let readFile path =
    let text = File.ReadAllText(path)
    let separator = System.Environment.NewLine + System.Environment.NewLine
    text.Split([| separator |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Trim())


let processGroup (groupData: string) = 
   groupData.ToCharArray()
   |> Array.fold(fun (res: Set<char>) (ch: char) -> if ch >= 'a' then res.Add ch else res ) Set.empty<char>


let processGroup2 (groupData: string) = 
   groupData.Split([| System.Environment.NewLine |], System.StringSplitOptions.None)
   |> Array.map (fun x -> Set.ofArray (x.ToCharArray()))
   |> Array.fold Set.intersect (Set(seq {'a' .. 'z'}))

let countAnswersForGroups path = 
   readFile path
   |> Array.map (processGroup2 >> Set.count )


