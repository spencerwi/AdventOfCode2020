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
        waypoint : Position
    }

    let initialState = {
        facing = 0
        position = (0, 0)
        waypoint = (10, 1)
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

    let moveShip ship (dx, dy) =
        let oldX, oldY = ship.position in
        { ship with position = (oldX + dx, oldY + dy) }

    let moveWaypoint ship (dx, dy) =
        let oldX, oldY = ship.waypoint in
        { ship with waypoint = (oldX + dx, oldY + dy) }

    let step1 (ship : ShipState) (action : Action) = 
        match action.movement with
        | Compass North -> moveShip ship (0, action.amount)
        | Compass South -> moveShip ship (0, action.amount * -1)
        | Compass East  -> moveShip ship (action.amount, 0)
        | Compass West  -> moveShip ship (action.amount * -1 , 0)
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
            | 0   -> moveShip ship (action.amount, 0)
            | 90  -> moveShip ship (0, action.amount)
            | 180 -> moveShip ship (action.amount * -1, 0)
            | 270 -> moveShip ship (0, action.amount * -1)
            | other -> failwith (sprintf "Ship was facing in a non-right-angle direction: %d" other)

    let step2 (ship : ShipState) (action : Action) = 
        match action.movement with
        | Compass North -> moveWaypoint ship (0, action.amount)
        | Compass South -> moveWaypoint ship (0, action.amount * -1)
        | Compass East  -> moveWaypoint ship (action.amount, 0)
        | Compass West  -> moveWaypoint ship (action.amount * -1 , 0)
        | Turn Left -> 
            let mutable currentWaypoint = ship.waypoint in
            let timesToRotate = action.amount / 90 in
            for _ in 1 .. timesToRotate do
                // to rotate counterclockwise, replace (x, y) with (-y, x)
                let wx, wy = currentWaypoint in
                currentWaypoint <- (-wy, wx)
            { ship with waypoint = currentWaypoint }
        | Turn Right -> 
            let mutable currentWaypoint = ship.waypoint in
            let timesToRotate = action.amount / 90 in
            for _ in 1 .. timesToRotate do
                // to rotate clockwise, replace (x, y) with (y, -x)
                let wx, wy = currentWaypoint in
                currentWaypoint <- (wy, -wx) 
            { ship with waypoint = currentWaypoint }
        | Forward -> 
            let wx, wy = ship.waypoint in
            moveShip ship (wx * action.amount, wy * action.amount)

    let run (stepper : ShipState -> Action -> ShipState) (actions : Action array) =
        actions
        |> Array.fold stepper initialState

end

let solve (lines : string array) =
    let actions = 
        lines
        |> Array.map Navigation.parseAction
    let runAndGetManhattanDistance stepper =
        let finalState = Navigation.run stepper actions in
        let x, y = finalState.position in
        (abs x) + (abs y) 
    in
    let part1Solution = runAndGetManhattanDistance Navigation.step1
    let part2Solution = runAndGetManhattanDistance Navigation.step2
    (part1Solution, part2Solution)



[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    let (part1Solution, part2Solution) = solve lines in
    printfn "Day 12A: %d" part1Solution
    printfn "Day 12B: %d" part2Solution
    0
