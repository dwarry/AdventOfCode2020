module Day1

open System.IO

let readFile (path: string) = 
  File.ReadAllLines(path)
    |> Array.map int
    |> Array.sort

let solve (path: string) = 
  let findMatch data = Array.find (fun x -> System.Array.BinarySearch(data, 2020 - x) > 0) data
  let calcResult first = (2020 - first) * first
  readFile path
    |> findMatch
    |> calcResult
    
let solve3 (path: string) = 
  let tailArray a n = 
    let aLength = Array.length a
    if aLength > n then
      Array.sub a n (aLength - n)
    else
      Array.empty

  let findSecond data target = Array.tryFind (fun x -> System.Array.BinarySearch(data, target - x) > 0) data
  let findMatch (data: int[]) = 
    data
      |> Array.mapi (fun i x -> 
           let first = data.[i]
           let target = 2020 - first
           let second = findSecond (tailArray data (i + 1)) target
           let third = Option.map (fun x -> target - x) second
           match second, third with
           | Some x, Some y -> Some (first * x * y)
           | _ -> None ) 
      |> Array.find Option.isSome

  readFile path 
    |> findMatch
        

