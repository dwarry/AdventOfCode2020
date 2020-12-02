module Day2

open System.IO
open System.Text.RegularExpressions

type PasswordRecord = {min: int; max: int; ch: char; password: string}

let parseLine (line: string) = 
  let m = Regex.Match(line, @"^(\d+)\-(\d+) ([a-z]): ([a-z]+)")
  if m.Success then
    Some { min = int m.Groups.[1].Value; 
           max = int m.Groups.[2].Value; 
           ch = m.Groups.[3].Value.[0]; 
           password = m.Groups.[4].Value }
  else
    None

let isValid (pswd: PasswordRecord): bool =
  let chCount = 
    pswd.password.ToCharArray() 
      |> Array.fold (fun count ch -> if ch = pswd.ch then (count + 1) else count) 0
  chCount >= pswd.min && chCount <= pswd.max

let isValid2 (pswd: PasswordRecord): bool = 
  (pswd.password.[pswd.min - 1] = pswd.ch) <> (pswd.password.[pswd.max - 1] = pswd.ch)

let readFile (path: string) =
  File.ReadAllLines(path) 
    |> Array.choose parseLine

let countValid (passwords: PasswordRecord[]) (predicate: PasswordRecord -> bool) =
  passwords |> Array.fold (fun count pswd -> if predicate pswd then count + 1 else count) 0


