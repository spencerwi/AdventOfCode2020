open System

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success 
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None

module Docking = begin
    type Rule = {
        index : int
        replacement : int
    }
    type Mask = Rule seq

    type ProgramState = {
        mask : Mask
        memory : Map<int64, int64>
    }

    let initialState = {
        mask = []
        memory = Map.empty
    }

    type Instruction = 
        | SetMask of Mask
        | UpdateMemory of int64 * int64

    let parseMask (bitmask : string) = 
        bitmask
        |> Seq.indexed 
        |> Seq.choose (fun (idx, c) -> 
            match c with
            | 'X' -> None
            | '0' -> Some { index = idx; replacement = 0 }
            | '1' -> Some { index = idx; replacement = 1 }
            | _ -> failwith ("Invalid bitmask char: " + string c)
        )

    let parseLine = function 
        | Regex "mask = ([X01]+)" [ mask ] -> SetMask (parseMask mask)
        | Regex "mem\[(\d+)\] = (\d+)" [ memLocationStr; rawValueStr ] ->
            UpdateMemory (int64 memLocationStr, int64 rawValueStr)
        | other -> failwith ("Invalid line: " + other)

    let applyRule (number : int64) (rule : Rule) : int64 =
        let mutable bits = Convert.ToString(number, 2).PadLeft(36, '0').ToCharArray() in
        bits.[rule.index] <- (string rule.replacement).[0]
        Convert.ToInt64(String(bits), 2)

    let applyMask (mask : Mask) (number : int64) : int64 =
        Seq.fold applyRule number mask

    let eval (state : ProgramState) (line : Instruction) = 
        match line with
        | SetMask m -> { state with mask = m }
        | UpdateMemory (location, rawValue) -> 
            let actualValue = applyMask state.mask rawValue in
            { state with memory = Map.add location actualValue state.memory }
end

let part1 (lines : string array) =
    let instructions = Array.map Docking.parseLine lines in
    let finalState = 
        instructions
        |> Array.fold Docking.eval Docking.initialState
    in
    Map.toSeq finalState.memory
    |> Seq.choose (function 
        | (_, 0L) -> None
        | (_, value) -> Some value
    )
    |> Seq.sum


let part2 (lines : string array) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
    let input = IO.File.ReadAllLines "input.txt" in
    printfn "Day 14A: %d" (part1 input)
//    printfn "Day 14B: %s" (part2 input)
    0
