open System

module Seating = begin
    type Grid = char [,]

    let doesSeatExist seatingGrid (row, col) =
        (
            row >= 0 && row < (Array2D.length1 seatingGrid)
            &&
            col >= 0 && col < (Array2D.length2 seatingGrid)
        )

    let isSeat = function
        | '#' | 'L' -> true
        | _ -> false

    let getAdjacentSpots (row, col) seatingGrid =
        seq {
            for i in (row - 1) .. (row + 1) do
                for j in (col - 1) .. (col + 1) do
                    if doesSeatExist seatingGrid (i, j) then
                        if (i, j) <> (row, col) then
                            yield (i, j)
        }

    let countAdjacentOccupiedSeats (seatingGrid : Grid) (row, col) =
        getAdjacentSpots (row, col) seatingGrid
        |> Seq.map (fun (row, col) -> seatingGrid.[row, col])
        |> Seq.filter (fun spot -> spot = '#')
        |> Seq.length

    let getVisibleSeats (row, col) seatingGrid =
        let directionsToSearch = [
            (-1, -1); (-1, 0); (-1, 1);
            (0,  -1);          ( 0, 1);
            (1,  -1); ( 1, 0); ( 1, 1)
        ]
        let rec searchInDirection (dRow, dCol) (spotRow, spotCol) =
            let adjacentSpot = (spotRow + dRow, spotCol + dCol) in
            if not (doesSeatExist seatingGrid adjacentSpot) then
                None
            else
                let adjacentValue = seatingGrid.[spotRow + dRow, spotCol + dCol] in
                if isSeat adjacentValue then
                    Some adjacentSpot
                else
                    searchInDirection (dRow, dCol) (spotRow + dRow, spotCol + dCol)
        in
        directionsToSearch
        |> Seq.map (fun d -> searchInDirection d (row, col))
        |> Seq.filter Option.isSome
        |> Seq.map Option.get

    let countVisibleOccupiedSeats (seatingGrid : Grid) (row, col) =
        getVisibleSeats (row, col) seatingGrid
        |> Seq.map (fun (row, col) -> seatingGrid.[row, col])
        |> Seq.filter (fun spot -> spot = '#')
        |> Seq.length

    let step1 (grid : Grid) : Grid =
        grid
        |> Array2D.mapi (fun row col spot ->
            let adjacentOccupiedSeats = countAdjacentOccupiedSeats grid (row, col) in
            match spot with
            | 'L' when adjacentOccupiedSeats = 0 ->  '#'
            | '#' when adjacentOccupiedSeats >= 4 -> 'L'
            | _ -> spot 
        )

    let step2 (grid : Grid) : Grid =
        grid
        |> Array2D.mapi (fun row col spot ->
            let visibleOccupiedSeats = countVisibleOccupiedSeats grid (row, col) in
            match spot with
            | 'L' when visibleOccupiedSeats = 0 ->  '#'
            | '#' when visibleOccupiedSeats >= 5 -> 'L'
            | _ -> spot 
        )

    let countAllOccupiedSeats (seatingGrid : Grid) =
        Seq.length (seq {
            for spot in Seq.cast<char> seatingGrid do
                if spot = '#' then yield spot
        })

    let runUntilStable (seatingGrid : Grid) (stepper : Grid -> Grid) =
        let mutable currentGrid = seatingGrid in
        let mutable stablePointFound = false in
        while not stablePointFound do
            let newGrid = stepper currentGrid in
            stablePointFound <- (newGrid = currentGrid)
            currentGrid <- newGrid
        currentGrid

end

let part1 (seatingGrid : Seating.Grid) =
    let stableGrid = Seating.runUntilStable seatingGrid Seating.step1
    Seating.countAllOccupiedSeats stableGrid

let part2 (seatingGrid : Seating.Grid) =
    let stableGrid = Seating.runUntilStable seatingGrid Seating.step2
    Seating.countAllOccupiedSeats stableGrid

[<EntryPoint>]
let main _ =
    let seatingGrid = array2D [
        for line in IO.File.ReadAllLines "input.txt" do
             line.ToCharArray() 
    ]
    printfn "Day 11A: %d" (part1 seatingGrid)
    printfn "Day 11B: %d" (part2 seatingGrid)
    0
