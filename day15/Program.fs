open System

let arrayLast (arr : 'a array) : 'a =
    if Array.isEmpty arr then
        failwith "Array is empty!"
    else
        arr.[arr.Length - 1]

let arrayDropLast (arr : 'a array) : 'a array =
    if Array.isEmpty arr then arr
    else
        arr.[0..(arr.Length - 2)]


let part1 (startingNumbers : int array) =
    let speak (alreadySpokenNumbers : int array) =
        let lastNumber = arrayLast alreadySpokenNumbers in
        let numbersUpToThatPoint = arrayDropLast alreadySpokenNumbers in
        let currentNumber = 
            if not (Array.contains lastNumber numbersUpToThatPoint) then
                0
            else
                let idx1 = Array.findIndexBack ((=) lastNumber) alreadySpokenNumbers in
                let subArrayUpToThatPoint = alreadySpokenNumbers.[0..(idx1 - 1)] in
                let idx2 = Array.findIndexBack ((=) lastNumber) subArrayUpToThatPoint in
                idx1 - idx2 
        in
        Array.append alreadySpokenNumbers [|currentNumber|]
    in
    let mutable startingList = startingNumbers in
    for _ in startingNumbers.Length .. 2019 do 
        startingList <- speak startingList 
    startingList.[2019] // Arrays are 0-indexed, so the 2020th word spoken is at index 2019

let part2 (input : string) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let startingNumbers = [|9;12;1;4;17;0;18|] in
    printfn "Day 15A: %d" (part1 startingNumbers)
//    printfn "Day 15B: %s" (part2 input)
    0
