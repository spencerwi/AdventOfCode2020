open System

module Seating = begin
    type Grid = char [,] // this is F# for "a 2d array of char"

    /// A convenience "is this in bounds" checker method.
    let doesSeatExist seatingGrid (row, col) =
        (
            row >= 0 && row < (Array2D.length1 seatingGrid)
            &&
            col >= 0 && col < (Array2D.length2 seatingGrid)
        )

    let isSeat = function
        | '#' | 'L' -> true
        | _ -> false

    /// <returns>
    /// All immediately-adjacent locations for a given (row, col) point 
    /// that are actually in-bounds on the grid.
    /// </returns>
    let getAdjacentSpots (row, col) seatingGrid =
        [
            (-1, -1); (-1, 0); (-1, 1);
            (0,  -1);          ( 0, 1);
            (1,  -1); ( 1, 0); ( 1, 1)
        ] 
        |> Seq.map (fun (dRow, dCol) -> (row + dRow, col + dCol))
        |> Seq.filter (doesSeatExist seatingGrid)

    /// <returns>
    /// The number of immediately-adjacent spots that are occupied seats.
    /// </returns>
    let countAdjacentOccupiedSeats (seatingGrid : Grid) (row, col) =
        getAdjacentSpots (row, col) seatingGrid
        |> Seq.map (fun (row, col) -> seatingGrid.[row, col])
        |> Seq.filter (fun spot -> spot = '#')
        |> Seq.length

    /// <returns>
    /// All the seats (occupied *or* unoccupied) that are in straight-line 
    /// line-of-sight from the given location in all 8 directions, taking 
    /// into account that some "sightlines" will *not* have contain a seat.
    /// </returns>
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

    /// <returns>
    /// The number of seats visible from a given spot (see getVisibleSeats) that 
    /// are occupied.
    /// </returns>
    let countVisibleOccupiedSeats (seatingGrid : Grid) (row, col) =
        getVisibleSeats (row, col) seatingGrid
        |> Seq.map (fun (row, col) -> seatingGrid.[row, col])
        |> Seq.filter (fun spot -> spot = '#')
        |> Seq.length

    /// <summary>
    /// Steps forward in time one "step" based on the rules in Part 1 of the puzzle.
    /// </summary>
    /// <returns>
    /// A new Grid that represents the state of the world after 1 time unit.
    /// </returns>
    let step1 (grid : Grid) : Grid =
        grid
        |> Array2D.mapi (fun row col spot ->
            let adjacentOccupiedSeats = countAdjacentOccupiedSeats grid (row, col) in
            match spot with
            | 'L' when adjacentOccupiedSeats = 0 ->  '#'
            | '#' when adjacentOccupiedSeats >= 4 -> 'L'
            | _ -> spot 
        )
    /// <summary>
    /// Steps forward in time one "step" based on the rules in Part 2 of the puzzle.
    /// </summary>
    /// <returns>
    /// A new Grid that represents the state of the world after 1 time unit.
    /// </returns>
    let step2 (grid : Grid) : Grid =
        grid
        |> Array2D.mapi (fun row col spot ->
            let visibleOccupiedSeats = countVisibleOccupiedSeats grid (row, col) in
            match spot with
            | 'L' when visibleOccupiedSeats = 0 ->  '#'
            | '#' when visibleOccupiedSeats >= 5 -> 'L'
            | _ -> spot 
        )

    /// <returns>
    /// All occupied seats anywhere in the given Grid.
    /// </returns>
    let countAllOccupiedSeats (seatingGrid : Grid) =
        Seq.length (seq {
            for spot in Seq.cast<char> seatingGrid do
                if spot = '#' then yield spot
        })

    /// <summary>
    /// Continuously steps through time, starting with the provided Grid and
    /// using the provided stepper function for each step, until a "stable point"
    /// where nothing in the Grid changes anymore is found.
    /// </summary>
    /// <returns>
    /// A Grid that represents the world at at that stable point.
    /// </returns>
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
