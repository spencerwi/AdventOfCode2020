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

/// Convenience method for checking if a given bus departs at a given timestamp
let doesBusDepartAt (bus : int64) (time : int64) =
    time % bus = 0L

/// Given a start time, and a multiplier to step by, find 
/// next time that the given next bus leaves, stepping in
/// multiples of "step".
let findTime start step nextBus nextBusOffset =
    let times = seq {
        for time in start .. step .. Int64.MaxValue do
            yield int64 time
    }
    times
    |> Seq.find (fun time -> doesBusDepartAt nextBus (time + nextBusOffset))

/// The way this solution works is by "locking in" patterns from
/// left to right -- from working smaller problems by hand, I noticed 
/// that each time we find a pattern between two numbers where they match
/// (that is, for x and y, a point where x*m + offset = y*n), then that
/// "pattern" only repeats when both numbers "line up" again later by hitting 
/// another multiple of *both* of them multiplied together after our offset. 
///
/// So when we find that for [5;7;13] that 5 * 4 = 20 and 7 * 3 = 21, we've
///  found a "pattern" to lock in: our 5-multiple and our 7-multiple line up.
///  So we start from 20, and count up by (5 * 7) to make sure our pattern stays
///  "locked in", until we find a spot where a multiple of 13 is then 2 more than
///  one of the pattern's repetitions.
///
/// After solving this and looking online, apparently this is something related to
/// "chinese number theorem", but reading over it kinda sailed over my head.
let part2 (buses : string array) =
    let busesWithOffsets = 
        buses
        |> Array.indexed
        |> Array.choose (fun (idx, value) ->
            if value = "x" then None
            else Some (int64 idx, int64 value)
        )
    in
    let (answer, _) = 
        busesWithOffsets
        |> Array.fold (fun (start, step) (nextBusOffset, nextBusId) -> 
            let newStart = findTime start step nextBusId nextBusOffset in
            (newStart, step * nextBusId)
        ) (0L, 1L)
    in
    answer

[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    let departureTime = Array.head lines |> int64 in
    let buses = lines.[1].Split(",", StringSplitOptions.RemoveEmptyEntries) in 
    printfn "Day 13A: %d" (part1 departureTime buses)
    printfn "Day 13B: %d" (part2 buses)
    0
