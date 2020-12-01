open System

let part1 (numbers : int seq) =
    let a, b = Seq.allPairs numbers numbers |> Seq.find (fun (x, y) -> x + y = 2020) in
    a * b

let part2 (numbers : int seq) =
    let generateCombinationsOf3 l = seq {
        for x in l do
        for y in l do
        for z in l do
        yield x, y, z
    }
    let a, b, c = generateCombinationsOf3 numbers |> Seq.find (fun (x, y, z) -> x + y + z = 2020) in
    a * b * c
    
    


[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllText "input.txt" in
    let lines = input.Split '\n' |> Array.filter (String.IsNullOrEmpty >> not) in
    let numbers = lines |> Array.map Int32.Parse |> Seq.ofArray in
    printfn "Day 1A: %d" (part1 numbers)
    printfn "Day 2A: %d" (part2 numbers)
    0
