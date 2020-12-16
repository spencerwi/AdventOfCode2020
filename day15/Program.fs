open System

let arrayLast (arr : 'a array) : 'a =
    if Array.isEmpty arr then
        failwith "Array is empty!"
    else
        arr.[arr.Length - 1]


let solve (startingNumbers : int array) =
    let mutable occurrencesMap : Map<int, int list> = 
        startingNumbers
        |> Array.indexed
        |> Array.fold (fun map (idx, item) -> 
            map
            |> Map.change item (function 
                | None -> Some [ idx ]
                | Some l -> Some (idx :: l)
            ) 
        ) Map.empty
    let wasNumberSeenBeforeLastTurn (number : int) (currentTurn : int) = 
        match Map.tryFind number occurrencesMap with
        | None -> false
        | Some [] -> false
        | Some l -> 
            let occurrencesBeforeGivenTurn = 
                List.filter (fun o -> o < (currentTurn - 1)) l
            in
            not (List.isEmpty occurrencesBeforeGivenTurn)
    in
    let markNumberOccurrence (number : int) (index : int) =
        occurrencesMap <- 
            occurrencesMap
            |> Map.change number (function
                | None -> Some [ index ]
                | Some l -> Some [index; l.Head]
            )
    let speak (lastSpokenNumber : int) (index : int) =
        let currentNumber = 
            if not (wasNumberSeenBeforeLastTurn lastSpokenNumber index) then
                0
            else
                let [idx2;idx1] = 
                    occurrencesMap
                    |> Map.find lastSpokenNumber 
                in
                idx2 - idx1
        in
        markNumberOccurrence currentNumber index
        currentNumber
    in
    let mutable lastNumber = 18
    for idx in startingNumbers.Length .. 2019 do 
        lastNumber <- speak lastNumber idx
    let part1Solution = lastNumber
    for idx in 2020 .. 29999999 do 
        lastNumber <- speak lastNumber idx
    (part1Solution, lastNumber)
    
[<EntryPoint>]
let main _ =
    let startingNumbers = [|9;12;1;4;17;0;18|] in
    let part1Solution, part2Solution = solve startingNumbers in
    printfn "Day 15A: %d" part1Solution
    printfn "Day 15B: %d" part2Solution
    0
