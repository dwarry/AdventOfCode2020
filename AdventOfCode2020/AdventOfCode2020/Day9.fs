module Day9

open System.IO

let readFile path = File.ReadAllLines(path) |> Array.map int64

let windowLength = 25

let windowsAndValues (lines: int64[]) = 
    let windows = seq {
        for i in 0 .. ((Array.length lines) - (1 + windowLength)) -> lines.[i..(i + windowLength - 1)]
    }
    let vals = lines.[windowLength..] |> Seq.ofArray
    Seq.zip windows vals

let log obj = printf "%A\n" obj; obj

let isInvalid (windowAndValue: int64[] * int64) = 
    let window, value = windowAndValue
    let windowSet = Set.ofArray window
    Set.filter (fun x -> (value - x <> x) && (Set.contains (value - x) windowSet )) windowSet
    |> Set.isEmpty

let part1 numbers = 
    numbers
    |> windowsAndValues
    |> Seq.filter isInvalid
    |> Seq.head
    |> snd

    
let part2 path = 
    let numbers = readFile path
    let target = part1 numbers
    let numbersEndIndex = (Array.length numbers) - 1
    let result lbound ubound = 
      printf "%d->%d\n" lbound ubound
      let rng = numbers.[lbound..ubound]
      (Array.min rng) + (Array.max rng)


    let rec findContiguous start current (tot: int64) =
       match start, current with
       | x, _  when x >= (numbersEndIndex - 1) -> None
       | _, y  when y > numbersEndIndex        -> findContiguous (start + 1) (start + 2) numbers.[start + 1]
       | _                                     -> let tot' = tot + numbers.[current]
                                                  if tot' = target then 
                                                    Some (result start current)
                                                  else if tot' > target then
                                                    findContiguous (start + 1) (start + 2) numbers.[start + 1]
                                                  else
                                                    findContiguous start (current + 1) tot'
    findContiguous 0 1 numbers.[0]
       