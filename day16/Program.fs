open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success 
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None


module Ticketing = begin
    type Field = {
        name : string
        isValid : int -> bool
    }

    type Ticket = int list

    let findInvalidValues (fields : Field array) (ticket : Ticket) : int seq =
        seq {
            for value in ticket do
                let isValidForAnyField = 
                    fields |> Array.exists (fun field -> field.isValid value) 
                in
                if not isValidForAnyField then
                    yield value
        }
    
    module Parsing = begin
        let parseField = function
            | Regex "([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" [ fieldName; d1; d2; d3; d4 ] ->
                {
                    name = fieldName
                    isValid = (fun x ->
                        ((int d1) < x && x < (int d2))
                        ||
                        ((int d3) < x && x < (int d4))
                    )
                }
            | other -> failwith ("Invalid field definition: " + other)

        let parseTicket (line : string) =
            line.Split(",", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> List.ofArray

    end
end

let part1 (fields : Ticketing.Field array) (otherTickets : Ticketing.Ticket array) =
    Seq.sum (seq {
        for ticket in otherTickets do
            yield! Ticketing.findInvalidValues fields ticket
    })

let part2 (input : string) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllText "input.txt" in
    let [|fieldsSection; yourTicketSection; otherTicketsSection|] = 
        Regex.Split(input, "\n\n", RegexOptions.Multiline)
    in
    let fields = 
        fieldsSection.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.skip 1
        |> Array.map Ticketing.Parsing.parseField
    let otherTickets = 
        otherTicketsSection.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.skip 1
        |> Array.map Ticketing.Parsing.parseTicket

    printfn "Day 16A: %d" (part1 fields otherTickets)
//    printfn "Day 16B: %s" (part2 input)
    0
