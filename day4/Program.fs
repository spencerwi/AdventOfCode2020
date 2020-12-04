open System
open System.Text.RegularExpressions

module Height = begin
    type T = 
        | Centimeters of int
        | Inches of int

    let parse (input : string) = 
        if input.Contains "in" then
            Some (Inches (input.Replace("in", "") |> int))
        else if input.Contains "cm" then
            Some (Centimeters (input.Replace("cm", "") |> int))
        else None // it's possible for the listed height to lack units or to use invalid units
end

module Passport = begin
    type T = {
        byr: int option
        iyr: int option
        eyr: int option
        hgt: Height.T option
        hcl: string option
        ecl: string option
        pid: string option
        cid: string option
    }

    let parsePassport (lines : string seq) = 
        let inputLine = String.concat " " lines in
        let fields = inputLine.Split ' ' in
        // convert the key:value strings into a true map 
        let fieldMapping = 
            Map.ofSeq (seq {
                for keyAndValue in fields do
                    let [|key; value|] = keyAndValue.Split ':'
                    yield (key, value)
            })
        {
            byr = (Map.tryFind "byr" fieldMapping |> Option.map int)
            iyr = (Map.tryFind "iyr" fieldMapping |> Option.map int)
            eyr = (Map.tryFind "eyr" fieldMapping |> Option.map int)
            hgt = (Map.tryFind "hgt" fieldMapping |> Option.bind Height.parse)
            hcl = Map.tryFind "hcl" fieldMapping
            ecl = Map.tryFind "ecl" fieldMapping
            pid = Map.tryFind "pid" fieldMapping
            cid = Map.tryFind "cid" fieldMapping
        }

    let parseInputFile filename =
        let input = System.IO.File.ReadAllText "input.txt" in
        Regex.Split(input, "\n\n", RegexOptions.Multiline) // each passport is multiline, separated by blank lines
        |> Array.map (fun group -> group.Split("\n", StringSplitOptions.RemoveEmptyEntries) |> Seq.ofArray)
        |> Array.map parsePassport


    let hasAllRequiredFields passport = 
        not (
            passport.byr = None 
            ||
            passport.iyr = None
            ||
            passport.eyr = None
            ||
            passport.hgt = None
            ||
            passport.hcl = None
            ||
            passport.ecl = None
            ||
            passport.pid = None
        ) // cid is optional

    let allFieldValuesAreValid passport =
        let rules = [
            (fun p -> 1920 <= p.byr.Value && p.byr.Value <= 2002);
            (fun p -> 2010 <= p.iyr.Value && p.iyr.Value <= 2020);
            (fun p -> 2020 <= p.eyr.Value && p.eyr.Value <= 2030);
            (fun p -> match p.hgt.Value with 
                        | Height.Centimeters c -> 150 <= c && c <= 193
                        | Height.Inches i -> 59 <= i && i <= 76
            );
            (fun p -> Regex.IsMatch(p.hcl.Value, "^#[0-9a-f]{6}$"));
            (fun p -> match p.ecl.Value with 
                        | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
                        | _ -> false
            );
            (fun p -> Regex.IsMatch(p.pid.Value, "^[0-9]{9}$"))
        ]
        List.forall (fun rule -> rule passport) rules

end

(** Since the part 2 solution uses the results of part 1, we just have a single
 * "solve" method that returns a tuple of the two parts' answers *)
let solve (filename : string) =
    let allPassports = Passport.parseInputFile filename 
    // This is more straightforward to read as a filter, then another filter, but it's more 
    // efficient as a single fold, since we only need to iterate once.
    let part1Solution, part2Solution = 
        allPassports
        |> Seq.fold (fun (fieldsPresentCount, validFieldsCount) passport ->
            if Passport.hasAllRequiredFields passport then
                if Passport.allFieldValuesAreValid passport then 
                    (fieldsPresentCount + 1, validFieldsCount + 1)
                else
                    (fieldsPresentCount + 1, validFieldsCount)
            else
                (fieldsPresentCount, validFieldsCount)
        ) (0, 0)
    (part1Solution, part2Solution)

[<EntryPoint>]
let main _ =
    let part1Solution, part2Solution = solve "input.txt" in
    printfn "Day 4A: %d" part1Solution
    printfn "Day 4B: %d" part2Solution
    0
