module Day3

open System.IO


let readFile (path: string) = 
    File.ReadAllLines(path)

let countTreeCollisions (map: string[]) (deltaX: int) (deltaY: int) = 
  let width = map.[0].Length
  let rec step x y n = 
    if y >= Array.length map then
      n
    else
      let newN = if map.[y].[x] = '#' then n + 1 else n
      let newX = ((x + deltaX) % width)
      let newY = (y + deltaY)
      step newX newY newN
  step 0 0 0


let part2 (path: string) = 
  let map = readFile path
  [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
  |> List.map (fun p -> let deltaX, deltaY = p
                        countTreeCollisions map deltaX deltaY)
  |> List.map int64
  |> List.fold (fun acc n -> acc * n) 1L
                        

