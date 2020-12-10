open System

type PathCount = 
    | NotYetComputed
    | Computed of int64
let forceUnwrapPathCount = function
    | Computed i -> i
    | NotYetComputed -> failwith "Tried to unwrap a not-yet-computed path count!"
type Adapter = {
    value : int
    mutable children : int seq
    // Cache the number of paths as we calculate so we don't have to keep recalculating them
    mutable numberOfPathsFromAdapter : PathCount
}

let rec seqTakeUpTo count s =
    if Seq.length s >= count then
        Seq.take count s
    else
        seqTakeUpTo (count - 1) s

let solve (joltages : int seq) =
    let deviceAdapterRating = (Seq.max joltages) + 3 in

    // We don't include the plug here because it's the "seed" element in our folds
    let sortedJoltages = seq {
        yield! Seq.sort joltages
        yield deviceAdapterRating
    }
    let differences = 
        sortedJoltages 
        |> Seq.fold (fun (count1, count2, count3, prev) next -> 
            match next - prev with
            | 1 -> (count1 + 1, count2, count3, next)
            | 2 -> (count1, count2 + 1, count3, next)
            | 3 -> (count1, count2, count3 + 1, next)
            | n -> failwith ("Too large of a joltage difference: " + string n)
        ) (0, 0, 0, 0)
    in
    let (oneJoltDiffs, _, threeJoltDiffs, _) = differences in
    let part1Solution = (oneJoltDiffs * threeJoltDiffs) in

    // Build up a map from adapter joltage to Adapter node (so that we can
    //  easily walk the tree later without having to do a bunch of up-front
    //  tree-building)
    let existingNodesMap : Map<int, Adapter> = 
        sortedJoltages
        |> Seq.fold (fun map adapter ->
            // since our sequence is sorted, and the gap between any two adapters 
            // is guaranteed to be <= 3, we don't have to worry about sliding 
            // windows around or doing any sorta takeWhile; just filter the list down!
            let children = 
                sortedJoltages
                |> Seq.filter (fun x -> x > adapter && x <= adapter + 3)
            in
            let adapterNode = {
                value = adapter
                children = children
                numberOfPathsFromAdapter = NotYetComputed
            }
            Map.add adapter adapterNode map
        ) Map.empty
    in
    // Having pseudo-built our tree, let's define a way to walk down 
    // from any given adapter and count the number of paths along the way
    let rec countPaths adapter =
        if Seq.isEmpty adapter.children 
        then 1L // there's only one path on this adapter: itself, alone
        else
            adapter.children 
            |> Seq.fold (fun pathCount current ->
                let currentNode = Map.find current existingNodesMap in
                if (currentNode.numberOfPathsFromAdapter = NotYetComputed) then
                    currentNode.numberOfPathsFromAdapter <- Computed (countPaths currentNode)
                pathCount + (forceUnwrapPathCount currentNode.numberOfPathsFromAdapter)
            ) 0L

    let part2Solution = countPaths (Map.find 0 existingNodesMap) in
    (part1Solution, part2Solution)

[<EntryPoint>]
let main _ =
    let input = seq {
        for line in IO.File.ReadAllLines "input.txt" do
            yield int line
    }
    let part1Solution, part2Solution = solve input in
    printfn "Day 10A: %d" part1Solution
    printfn "Day 10B: %d" part2Solution
    0
