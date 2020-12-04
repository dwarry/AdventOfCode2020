module Day4

open System.IO
open System.Text.RegularExpressions

let mandatoryFields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; ]

let validEyeColors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let readFile (path) = 
    File.ReadAllLines(path)

let parseFile (path: string) =
    let addToMap (currentMap: Map<string, string>) (kv: string) = 
        let parts = kv.Split(':')
        currentMap.Add(parts.[0].Trim(), parts.[1])

    let addLineToMap (currentMap: Map<string, string>) (s: string) = 
        s.Split(' ') |> Array.fold addToMap currentMap 
                                             

    let parseLine (state: Map<string, string> * List<Map<string,string>>) (line: string) = 
        match state with 
        | (currentMap, results) when String.length line = 0 -> (Map.empty, currentMap :: results)
        | (currentMap, results) -> (addLineToMap currentMap line, results)

    readFile path
    |> Array.fold parseLine (Map.empty<string, string>, [])
    |> snd

let isValid (m: Map<string, string>) = 
    match m.Count with
    | 7 -> not (m.ContainsKey "cid")
    | 8 -> true
    | _ -> false
    //mandatoryFields |> List.forall (fun x -> m.ContainsKey x)

let isValid2 (m: Map<string, string>) = 
  let validationRules = Map.ofList [ 
      ("byr", fun x -> Regex.IsMatch(x, @"^\d{4}$") && x >= "1920" && x <= "2002") 
      ("iyr", fun x -> Regex.IsMatch(x, @"^\d{4}$") && x >= "2010" && x <= "2020")
      ("eyr", fun x -> Regex.IsMatch(x, @"^\d{4}$") && x >= "2020" && x <= "2030")
      ("hgt", fun x -> if Regex.IsMatch(x, @"^(?:\d{2}in|\d{3}cm)$") then
                           let h = x.Substring(0, x.Length - 2) 
                           if x.Substring(x.Length - 2) = "in" then
                             h >= "59" && h <= "76"
                           else
                             h >= "150" && h <= "193"
                        else false)
      ("hcl", fun x -> Regex.IsMatch(x, @"^#[0-9a-f]{6}$"))
      ("ecl", fun x -> List.exists (fun y -> y = x) validEyeColors)
      ("pid", fun x -> Regex.IsMatch(x, @"^\d{9}$"))
      ("cid", fun x -> true)
    ]

  Map.forall (fun k v -> 
                  let result = validationRules.[k] v
                  printf "%s %s %s\n" k v (result.ToString())
                  result
                  ) m
  

let countValidEntries (path: string) = 
    parseFile path
    |> List.filter isValid
    |> List.filter isValid2
    |> List.length



