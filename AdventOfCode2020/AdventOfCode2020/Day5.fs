module Day5

open System.IO

let readFile path = 
  File.ReadAllLines(path)

// description is really just a convoluted way of describing binary where B and R are 1; F and L = 0
// Was going to use System.Collections.Specialized.BitVector32 to do the bit-twiddling, 
// but couldn't get F# to assign to the Item property. 
let findSeat (locator: string) = 
    locator.ToCharArray()
    |> Array.mapi (fun i ch -> let pow = locator.Length - 1 - i
                               let v = if (ch = 'B' || ch = 'R') then 1 else 0
                               (v <<< pow))
    |> Array.fold (+) 0
      
let findMaxSeat path = 
    path 
    |> readFile 
    |> Array.map findSeat
    |> Array.max

let findMySeat path = 
    path
    |> readFile
    |> Array.map findSeat
    |> Array.sort
    |> Array.pairwise
    |> Array.find (fun p -> (snd p) - (fst p) = 2)
    |> (fun p -> (fst p) + 1)

