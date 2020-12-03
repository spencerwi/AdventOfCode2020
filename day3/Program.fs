module SkiSlope = begin
    type T = string array
    type Path = {right : int; down: int}

    let isTree (c : char) = c = '#'

    let countTreesForPath (slope : T) (path: Path) = 
        let pointsAlongPath = seq {
            let x = ref 0
            for y in 0 .. path.down .. (slope.Length - 1) do
                yield slope.[y].[!x]
                x := (!x + path.right) % slope.[y].Length
        } 
        pointsAlongPath
        |> Seq.filter isTree
        |> Seq.length
end

let part1 (lines : SkiSlope.T) =
    SkiSlope.countTreesForPath lines {right = 3; down = 1}

let part2 (lines : SkiSlope.T) =
    let paths = [
        {SkiSlope.right = 1; SkiSlope.down = 1};
        {SkiSlope.right = 3; SkiSlope.down = 1};
        {SkiSlope.right = 5; SkiSlope.down = 1};
        {SkiSlope.right = 7; SkiSlope.down = 1};
        {SkiSlope.right = 1; SkiSlope.down = 2};
    ] 
    paths
    |> List.map (SkiSlope.countTreesForPath lines)
    |> List.fold (*) 1
    
[<EntryPoint>]
let main _ =
    let lines = System.IO.File.ReadAllLines "input.txt" in
    printfn "Day 3A: %d" (part1 lines)
    printfn "Day 3B: %d" (part2 lines)
    0
