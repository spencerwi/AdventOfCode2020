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
    let numbers = seq {
        for line in IO.File.ReadAllLines "input.txt" do
        if not (String.IsNullOrEmpty line) then
            yield int line
    }
    printfn "Day 1A: %d" (part1 numbers)
    printfn "Day 1B: %d" (part2 numbers)
    0
