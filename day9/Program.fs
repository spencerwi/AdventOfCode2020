open System

// This approach is pretty inefficient, but thankfully it still runs quick 
// enough to solve the problem. It'd be better to quit calling "tail" so 
// many times and just slide a window or use slicing. Still, this runs
// quickly enough, so it's good enough for me.
//

/// Efficiently finds both the min *and* max of an array in one pass
let arrayMinAndMax arr = 
    let (min, max) = 
        Array.fold (fun (min, max) next -> 
            let newMin = 
                match min with
                | None -> Some next
                | Some existingMin -> Some (Seq.min [existingMin; next])
            let newMax =
                match max with
                | None -> Some next
                | Some existingMax -> Some (Seq.max [existingMax; next])
            (newMin, newMax)
        ) (None, None) arr
    match (min, max) with
    | (None, _) | (_, None) -> failwith "Something went wrong: could not determine max or min!"
    | (Some min, Some max) -> (min, max)

module XMASDecryption = begin
    let isValidNextNumber (incomingNumber : int64) (window : int64 seq) =
        let allPairwiseCombinations = seq {
            for x in window do
            for y in window do
                if x <> y then
                    yield (x, y)
        } 
        Seq.exists (fun (x, y) -> x + y = incomingNumber) allPairwiseCombinations
end

let solve (lines : int64 seq) =
    // A recursive function is idiomatic in OCaml/F# for stepping through 
    // lists in this way and stopping to return a value once some condition
    // is satisfied; See the "99 problems" on the OCaml website for examples.
    let rec stepThroughWindow fullInput =
        let window = Seq.take 25 fullInput in
        let nextNumber = Seq.item 25 fullInput in
        if not (XMASDecryption.isValidNextNumber nextNumber window) then
            nextNumber
        else
            stepThroughWindow (Seq.tail fullInput)
    in 
    let invalidNumber = stepThroughWindow lines in
    // Convert the seq of lines to an array so that we can index into it
    let linesArray = Seq.toArray lines in
    let mutable contiguousSetThatAddsUp = None in
    while contiguousSetThatAddsUp = None do
        for i = 0 to (linesArray.Length - 1) do
            for j = 1 to (linesArray.Length - (i + 1)) do
                let subsequence = linesArray.[i..j] in
                if (Seq.sum subsequence) = invalidNumber then
                    contiguousSetThatAddsUp <- Some subsequence

    let (smallest, largest) = arrayMinAndMax (Option.get contiguousSetThatAddsUp) in
    let encryptionWeakness = smallest + largest in
    (invalidNumber, encryptionWeakness)

[<EntryPoint>]
let main _ =
    let input = seq { 
        for line in IO.File.ReadAllLines "input.txt" do
            yield int64 line
    }
    let part1Solution, part2Solution = solve input in
    printfn "Day 9A: %d" part1Solution
    printfn "Day 9B: %d" part2Solution
    0
