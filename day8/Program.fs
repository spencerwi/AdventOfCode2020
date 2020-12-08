open System
open System.Runtime.CompilerServices

module GameConsole = begin
    type Operation = 
        | Acc
        | Jmp
        | Nop

    type Status =
        | Running
        | ProgramTerminated
        | LoopDetected

    [<IsReadOnly; Struct>]
    type Instruction = {
        operation : Operation
        argument : int
    }

    [<IsReadOnly; Struct>]
    type GameState = {
        accumulator : int64
        instructionPointer : int
    }

    let initialGameState = 
        { accumulator = 0L; instructionPointer = 0 }

    let eval gameState instruction =
        match instruction.operation with
        | Acc -> 
            { 
                accumulator = gameState.accumulator + (int64 instruction.argument)
                instructionPointer = gameState.instructionPointer + 1
            }
        | Jmp -> 
            { 
                gameState with instructionPointer = gameState.instructionPointer + instruction.argument
            }
        | Nop ->
            {
                gameState with instructionPointer = gameState.instructionPointer + 1
            }

    let parseInstruction (line : string) =
        match line.Split(" ") with
        | [|opString; argString|] -> 
            let operation = 
                match opString with
                | "acc" -> Acc
                | "jmp" -> Jmp
                | "nop" -> Nop
                | _ -> failwith ("Invalid operation: " + line)
            let argument = int argString in
            { operation = operation; argument = argument }
        | _ -> failwith ("Invalid line: " + line)

    /// Runs a given set of instructions until either the program terminates
    ///  (by moving the instruction pointer outside the program) or until a loop 
    ///  is detected.
    ///
    /// <returns>
    /// A tuple of <see cref="GameConsole.Status">the reason for halting</see> 
    /// and the final accumulator state
    /// </returns>
    let run (instructions : Instruction array) = 
        let mutable seenInstructions = Set.empty in
        let mutable gameState = initialGameState in
        let mutable status = Running in
        while status = Running do
            if gameState.instructionPointer > (instructions.Length - 2) then
                status <- ProgramTerminated
            else if Set.contains gameState.instructionPointer seenInstructions then
                status <- LoopDetected
            else
                seenInstructions <- Set.add gameState.instructionPointer seenInstructions
                let currentInstruction = instructions.[gameState.instructionPointer]
                gameState <- eval gameState currentInstruction
        (status, gameState)
end

// Problem statement: the given program is known to loop infinitely; find the
//  state of the accumulator when the program first starts to loop.
let part1 (instructions : GameConsole.Instruction array) =
    let (_, stateWhenLoopOccurs) = GameConsole.run instructions in
    stateWhenLoopOccurs.accumulator

// Problem statement: exactly one of the Jmp or Nop instructions should be 
//  flipped to either a Nop or a Jmp, respectively, in order to allow the 
//  program to terminate normally. Find that instruction, flip it correctly,
//  and return the final accumulator state after the program terminates 
//  normally.
let part2 (originalInstructions : GameConsole.Instruction array) =
    let mutable correctedFinalAccumulatorState = None in
    let mutable i = 0 in
    while (correctedFinalAccumulatorState = None) && (i < originalInstructions.Length) do
        let instruction = originalInstructions.[i] in
        // We only want to change a Jmp or a Nop; the Accs are fine
        if instruction.operation <> GameConsole.Acc then
            let workingCopy = Array.copy originalInstructions in
            let newOperation = 
                match instruction.operation with
                | GameConsole.Jmp -> GameConsole.Nop
                | GameConsole.Nop -> GameConsole.Jmp
            in
            workingCopy.[i] <- { instruction with operation = newOperation }
            let (result, finishedState) = GameConsole.run workingCopy in
            if result = GameConsole.ProgramTerminated then
                correctedFinalAccumulatorState <- Some finishedState.accumulator
        i <- i + 1 
    match correctedFinalAccumulatorState with
    | Some number -> number
    | None -> failwith "Something's wrong, no single fix got the program to terminate!"

[<EntryPoint>]
let main _ =
    let lines = IO.File.ReadAllLines "input.txt" in
    let instructions = Array.map GameConsole.parseInstruction lines in
    printfn "Day 8A: %d" (part1 instructions)
    printfn "Day 8B: %d" (part2 instructions)
    0
