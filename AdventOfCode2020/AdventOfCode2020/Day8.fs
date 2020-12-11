module Day8

open System.IO
open System.Diagnostics

let readFile path = 
    File.ReadAllLines(path)

let part1 (lines: string[]) = 
    let rec processLine (acc: int) (visitedLines: Set<int>) (index: int) = 
      if Set.contains index visitedLines then
        acc
      else
        match lines.[index].Split([|' '|], 2) with
        | [| "nop"; _ |] -> processLine acc (visitedLines.Add index) (index + 1)
        | [| "acc"; x |] -> processLine (acc + (int x)) (visitedLines.Add index) (index + 1)
        | [| "jmp"; x |] -> processLine acc (visitedLines.Add index) (index + (int x))
        | _ -> failwithf "Could not process line %d: %s" index lines.[index]
    processLine 0 Set.empty 0
    

// returns (lineNo, accumulator) for the successful edit. 
let part2 (originalLines: string[]) = 
    let rec processLine (lines: string[]) (acc: int) (visitedLines: Set<int>) (index: int) = 
      if Set.contains index visitedLines then
        None
      else if index >= Array.length lines then 
        Some acc
      else
        match lines.[index].Split([|' '|], 2) with
        | [| "nop"; _ |] -> processLine lines acc (visitedLines.Add index) (index + 1)
        | [| "acc"; x |] -> processLine lines (acc + (int x)) (visitedLines.Add index) (index + 1)
        | [| "jmp"; x |] -> processLine lines acc (visitedLines.Add index) (index + (int x))
        | _ -> failwithf "Could not process line %d: %s" index lines.[index]

    let fixLines n = 
        if originalLines.[n].[..2] = "acc" then 
            None
        else
            let newLines = Array.copy originalLines
            let newLine = newLines.[n]
            newLines.[n] <- (if newLine.[..2] = "jmp" then "nop" else "jmp") + newLine.[3..]
            Some (n, newLines)


    seq { for n in 0 .. ((Array.length originalLines) - 1) -> fixLines n}
      |> Seq.choose id
      |> Seq.choose (fun x -> match (processLine (snd x) 0 Set.empty 0) with 
                              | Some result -> Some ((fst x), result)
                              | None -> None)

    


