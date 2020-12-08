open System
open System.Text.RegularExpressions

/// <summary>
/// Like (+) but for pairs. 
/// </summary>
/// <returns>
/// (a1 + b1, a2 + b2)
/// </returns>
let addPair (a1, a2) (b1, b2) =
    (a1 + b1, a2 + b2)

let solve (input : string) =
    let groups = 
        Regex.Split(input, "\n\n", RegexOptions.Multiline)
        |> Array.map (fun group -> group.Split("\n", StringSplitOptions.RemoveEmptyEntries))
    in
    let groupTotals = seq {
        for group in groups do
            // We want each voter's yes responses as a set so we can unionMany and intersectMany.
            let eachVoterAsSet = seq {
                for voter in group do
                    let questionsVoterAnsweredYesTo = 
                        voter 
                        |> String.filter (not << Char.IsWhiteSpace) 
                        |> Set.ofSeq
                    in yield questionsVoterAnsweredYesTo
            } 
            let questionsAtLeastOnePersonAnsweredYesTo = Set.unionMany eachVoterAsSet |> Set.count in
            let questionsEveryoneAnsweredYesTo = Set.intersectMany eachVoterAsSet |> Set.count in
            yield (
                questionsAtLeastOnePersonAnsweredYesTo,
                questionsEveryoneAnsweredYesTo
            )
    }
    Seq.fold addPair (0,0) groupTotals 

[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllText "input.txt" in
    let (part1Solution, part2Solution) = solve input in
    printfn "Day 6A: %d" part1Solution 
    printfn "Day 6B: %d" part2Solution
    0
