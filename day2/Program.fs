open System

module Password = begin
    type t = { testChar : char; firstNum : int; secondNum : int; text : string }

    let isValid1 (password : t) = 
        let occurrences = password.text |> Seq.filter (fun c -> c = password.testChar) |> Seq.length in
        password.firstNum <= occurrences && occurrences <= password.secondNum

    let isValid2 (password : t) =
        let i, j = (password.firstNum - 1), (password.secondNum - 1) in
        let charsToCheck = Seq.ofList [password.text.[i]; password.text.[j]] in
        let numberOfMatches = charsToCheck |> Seq.filter (fun c -> c = password.testChar) |> Seq.length in
        numberOfMatches = 1

    let parse (inputString : string) =
        let segments = inputString.Split ' ' in
        let [|firstNum; secondNum|] = (segments.[0].Split '-') |> Array.map int in
        let testChar = Seq.head segments.[1] in
        let text = segments.[2] in
        { testChar = testChar; firstNum = firstNum; secondNum = secondNum; text = text }
end

let part1 (passwords : Password.t seq) =
    passwords
    |> Seq.filter Password.isValid1
    |> Seq.length

let part2 (passwords : Password.t seq) =
    passwords
    |> Seq.filter Password.isValid2
    |> Seq.length
    
[<EntryPoint>]
let main _ =
    let passwords = seq {
        for line in IO.File.ReadAllLines "input.txt" do
        if not (String.IsNullOrEmpty line) then
            yield Password.parse line
    }
    printfn "Day 2A: %d" (part1 passwords)
    printfn "Day 2B: %d" (part2 passwords)
    0
