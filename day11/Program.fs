open System

module Seating = begin
    type Grid = char [,]

    let doesSeatExist seatingGrid (row, col) =
        (
            row >= 0 && row < (Array2D.length1 seatingGrid)
            &&
            col >= 0 && col < (Array2D.length2 seatingGrid)
        )

    let getAdjacentSpots (row, col) seatingGrid =
        seq {
            for i in (row - 1) .. (row + 1) do
                for j in (col - 1) .. (col + 1) do
                    if doesSeatExist seatingGrid (i, j) then
                        if (i, j) <> (row, col) then
                            yield (i, j)
        }

    let isOccupiedSeat (seatingGrid : Grid) (row, col) =
        seatingGrid.[row, col] = '#'

    let countAdjacentOccupiedSeats (seatingGrid : Grid) (row, col) =
        getAdjacentSpots (row, col) seatingGrid
        |> Seq.filter (fun (x, y) -> isOccupiedSeat seatingGrid (x, y))
        |> Seq.length

    let step (grid : Grid) : Grid =
        grid
        |> Array2D.mapi (fun row col spot ->
            let adjacentOccupiedSeats = countAdjacentOccupiedSeats grid (row, col) in
            match spot with
            | 'L' when adjacentOccupiedSeats = 0 ->  '#'
            | '#' when adjacentOccupiedSeats >= 4 -> 'L'
            | _ -> spot 
        )

    let countAllOccupiedSeats (seatingGrid : Grid) =
        printfn "%A" seatingGrid
        Seq.length (seq {
            for spot in Seq.cast<char> seatingGrid do
                if spot = '#' then yield spot
        })

end

let part1 (seatingGrid : Seating.Grid) =
    let mutable currentGrid = seatingGrid in
    let mutable stablePointFound = false in
    while not stablePointFound do
        let newGrid = Seating.step currentGrid in
        stablePointFound <- (newGrid = currentGrid)
        currentGrid <- newGrid
    Seating.countAllOccupiedSeats currentGrid


let part2 (seatingGrid : Seating.Grid) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let seatingGrid = array2D [
        for line in IO.File.ReadAllLines "input.txt" do
             line.ToCharArray() 
    ]
    printfn "Day 11A: %d" (part1 seatingGrid)
//    printfn "Day 11B: %s" (part2 input)
    0
