open System

module Navigation = begin
    type Position = int * int
    type CompassDirection = 
        | North | South | East | West
    type RelativeDirection = 
        | Left | Right

    type Move = 
        | Compass of CompassDirection
        | Turn of RelativeDirection
        | Forward 

    type Action = {
        movement : Move
        amount : int
    }

    type ShipState = {
        facing : int // degrees
        position : Position
    }

    let initialState = {
        facing = 0
        position = (0, 0)
    }

    let parseAction (line : string) =
        let movement = 
            match line.[0] with
            | 'N' -> Compass North
            | 'S' -> Compass South
            | 'E' -> Compass East
            | 'W' -> Compass West
            | 'L' -> Turn Left
            | 'R' -> Turn Right
            | 'F' -> Forward
            | c -> failwith ("Unrecognized movement: " + string c)
        in 
        let amount = line.Substring(1) |> int
        in { movement = movement ; amount = amount }

    let updatePosition ship (dx, dy) =
        let oldX, oldY = ship.position in
        { ship with position = (oldX + dx, oldY + dy) }

    let step (ship : ShipState) (action : Action) = 
        match action.movement with
        | Compass North -> updatePosition ship (0, action.amount)
        | Compass South -> updatePosition ship (0, action.amount * -1)
        | Compass East -> updatePosition ship (action.amount, 0)
        | Compass West -> updatePosition ship (action.amount * -1 , 0)
        | Turn Left -> { ship with facing = (ship.facing + action.amount) % 360 }
        | Turn Right -> 
            let rawNewDirection = (ship.facing - action.amount) % -360 in
            let newDirection = 
                if rawNewDirection < 0 then
                    360 + rawNewDirection
                else
                    rawNewDirection
            in
            { ship with facing = newDirection }
        | Forward -> 
            match ship.facing with
            | 0 -> updatePosition ship (action.amount, 0)
            | 90 -> updatePosition ship (0, action.amount)
            | 180 -> updatePosition ship (action.amount * -1, 0)
            | 270 -> updatePosition ship (0, action.amount * -1)
            | other -> failwith (sprintf "Ship was facing in a non-right-angle direction: %d" other)

end

let part1 (lines : string array) =
    let finalState =
        lines
        |> Array.map Navigation.parseAction
        |> Array.fold Navigation.step Navigation.initialState
    in
    printfn "%A" finalState
    let x, y = finalState.position in
    (abs x) + (abs y)

let part2 (lines : string array) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    printfn "Day 12A: %d" (part1 lines)
//    printfn "Day 12B: %s" (part2 input)
    0
