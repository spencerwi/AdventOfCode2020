open System

let part1 (departureTime : int64) (buses : string array) =
    let inServiceBuses = 
        buses
        |> Array.choose (function
            | "x" -> None
            | s -> Some (int64 s)
        )
    let busChoice, busDeparture = 
        inServiceBuses
        |> Array.map (fun busId ->
            let earliestMatchingDepartureOnBus = 
                busId * ((departureTime / busId) + 1L)
            in
            (busId, earliestMatchingDepartureOnBus)
        )
        |> Array.minBy snd
    in
    busChoice * (busDeparture - departureTime)

let part2 (input : string) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    let departureTime = Array.head lines |> int64 in
    let buses = lines.[1].Split(",", StringSplitOptions.RemoveEmptyEntries) in 
    printfn "Day 13A: %d" (part1 departureTime buses)
//    printfn "Day 13B: %s" (part2 input)
    0
