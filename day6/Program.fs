open System
open System.Text.RegularExpressions

let solve (input : string) =
    let groups = 
        Regex.Split(input, "\n\n", RegexOptions.Multiline)
        |> Array.map (fun group -> group.Split("\n", StringSplitOptions.RemoveEmptyEntries))
    in
    let groupsWithEachVoterAsASet = 
        seq {
            for group in groups do
                let groupWithEachVoterAsASet = seq {
                    for voter in group do
                        yield Set.ofSeq (seq {
                            for questionVoterAnsweredYesTo in voter do
                                if not (Char.IsWhiteSpace questionVoterAnsweredYesTo) then
                                    yield questionVoterAnsweredYesTo
                        })
                }
                yield groupWithEachVoterAsASet
        }
    let groupCountsOfAllQuestionsAnyoneAnsweredYesTo =
        seq {
            for group in groupsWithEachVoterAsASet do
                yield (Set.unionMany group |> Set.count)
        }
    let part1Solution = Seq.sum groupCountsOfAllQuestionsAnyoneAnsweredYesTo in
    let groupCountsOfAllQuestionsEveryoneAnsweredYesTo =
        seq {
            for group in groupsWithEachVoterAsASet do
                yield (Set.intersectMany group |> Set.count)
        }
    let part2Solution = Seq.sum groupCountsOfAllQuestionsEveryoneAnsweredYesTo in
    (part1Solution, part2Solution)

[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllText "input.txt" in
    let (part1Solution, part2Solution) = solve input in
    printfn "Day 6A: %d" part1Solution 
    printfn "Day 6B: %d" part2Solution
    0
