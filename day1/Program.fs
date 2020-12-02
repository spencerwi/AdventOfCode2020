open System

let part1 (numbers : int seq) =
    Seq.head (seq {
        for x in numbers do
        for y in numbers do
        if x + y = 2020 then
            yield x * y
    })

let part2 (numbers : int seq) =
    Seq.head (seq {
        for x in numbers do
        for y in numbers do
        for z in numbers do
        if x + y + z = 2020 then
            yield x * y * z
    })
    

[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllText "input.txt" in
    let lines = input.Split '\n' |> Array.filter (String.IsNullOrEmpty >> not) in
    let numbers = lines |> Array.map Int32.Parse |> Seq.ofArray in
    printfn "Day 1A: %d" (part1 numbers)
    printfn "Day 1B: %d" (part2 numbers)
    0
