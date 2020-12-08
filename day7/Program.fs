open System

module LuggageProcessing = begin
    type ChildBagCounts = {
        quantity: uint64
        color: string
    }
    type Bag = {
        inputColor: string
        children: ChildBagCounts list
        mutable childBags : Bag list
    }

    let rec canContainColor (color : string) (bag : Bag) =
        if List.exists (fun o -> o.color = color) bag.children then true
        else
            List.exists (canContainColor color) bag.childBags

    let rec countBags (root : Bag) =
        if List.isEmpty root.children 
        then 
            1uL // just this 1 bag
        else
            let childSums = seq {
                for child in root.children do
                    let correspondingChildBag = List.find (fun bag -> bag.inputColor = child.color) root.childBags 
                    let childCount = (countBags correspondingChildBag) in
                    yield (child.quantity * childCount)
            }
            1uL + (Seq.sum childSums) // This 1 bag plus its children

    module Parsing = begin
        open FParsec

        let addChild (parent : Bag) (child : Bag) =
            parent.childBags <- (child :: parent.childBags)

        let parseBag (input : string) =
            let inputColorParser = manyCharsTill anyChar (pstring " bags contain ") in
            let individualChildParser = (puint64 .>> spaces) .>>. manyCharsTill anyChar ((pstring " bags") <|> (pstring " bag")) in
            let multipleChildParser = sepBy individualChildParser (pstring ", ") .>> pchar '.'
            let noChildrenParser = (pstring "no other bags.") >>% []
            let bagParser = (inputColorParser .>>. (noChildrenParser <|> multipleChildParser)) |>> (fun (inputColor, children) -> 
                let parsedChildren = children |> List.map (fun (quantity, color) -> 
                    { quantity = quantity; color = color.Trim() }
                )
                { inputColor = inputColor.Trim(); children = parsedChildren; childBags = [] }
            )
            match run bagParser input with
            | ParserResult.Success (bag, _, _) -> bag
            | ParserResult.Failure (msg, _, _) -> failwith msg

        let parseBagTrees (inputLines : string array) = 
            let allBags = Array.map parseBag inputLines in
            let attachChildren bag =
                if (bag.children <> []) then
                    for children in bag.children do
                        match Array.tryFind (fun r -> r.inputColor = children.color) allBags with 
                        | Some child -> addChild bag child
                        | _ -> ()
            Array.iter attachChildren allBags
            allBags
    end
end

let solve (lines : string array) =
    let bagTrees = LuggageProcessing.Parsing.parseBagTrees lines in
    let colorsThatCanContainShinyGold = 
        bagTrees
        |> Array.filter (LuggageProcessing.canContainColor "shiny gold") 
        |> Array.length
    in
    let shinyGoldBag = 
        Array.find (fun (r : LuggageProcessing.Bag) -> r.inputColor = "shiny gold") bagTrees
    in
    let childBagsRequiredForEachShinyGold =
        (LuggageProcessing.countBags shinyGoldBag) - 1uL
    in
    (colorsThatCanContainShinyGold, childBagsRequiredForEachShinyGold)

[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    let part1Solution, part2Solution = solve lines in
    printfn "Day 7A: %d" (part1Solution)
    printfn "Day 7B: %d" (part2Solution)
    0
