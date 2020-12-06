open System

let midpoint x y =
    (x + y) / 2

module Seating = begin
    type Seat = {
        Row : int
        Column : int
        Id : int
    }

    let findSeatFromBoardingPass boardingPass = 
        let binarySearch (lowChar, highChar) (startMin, startMax) chars =
            chars
            |> Seq.fold (fun (min, max) c ->
                let middle = midpoint min max in
                if      c = lowChar  then (min, middle)
                else if c = highChar then (middle + 1, max)
                else failwith ("Invalid char: " + (string c))
            ) (startMin, startMax)
            |> fst
        in
        let row = 
            boardingPass
            |> Seq.take 7
            |> binarySearch ('F', 'B') (0, 127)  
        let col =
            boardingPass
            |> Seq.skip 7
            |> binarySearch ('L', 'R') (0, 7)  
        { Row = row; Column = col; Id = (row * 8) + col }
end

let solve (lines : string seq) =
    let seatIds = seq {
        for boardingPass in lines do
            let seat = Seating.findSeatFromBoardingPass boardingPass in
            yield seat.Id
    }
    let contiguousSeatIds = Seq.sort seatIds in
    let highestSeatId = Seq.last contiguousSeatIds in
    let beforeMissingSeat, afterMissingSeat =
        Seq.pairwise contiguousSeatIds
        |> Seq.find (fun (seat1Id, seat2Id) -> (seat2Id - seat1Id) > 1)
    in
    let missingSeatId = midpoint beforeMissingSeat afterMissingSeat in
    (highestSeatId, missingSeatId)
    

[<EntryPoint>]
let main _ =
    let lines = seq {
        for line in IO.File.ReadAllLines "input.txt" do
        if not (String.IsNullOrEmpty line) then
            yield line
    }
    let (part1Solution, part2Solution) = solve lines in
    printfn "Day 5A: %d" part1Solution
    printfn "Day 5B: %d" part2Solution
    0
