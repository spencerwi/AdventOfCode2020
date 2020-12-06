#!/bin/bash

if [ -z "$1" ] ; then
	echo "USAGE: newday.sh DAYNUM"
	exit 1;
fi

if [ -d "day$1" ] ; then
	echo "day$1 already exists!"
	exit 1
fi;

mkdir "day$1"
cd "day$1"
dotnet new console -lang 'f#'
cat <<EOF >Program.fs 
open System

let part1 (input : string) =
    "not yet implemented!"

let part2 (input : string) =
    "not yet implemented!"

[<EntryPoint>]
let main _ =
	let input = IO.File.ReadAllText "input.txt" in
    printfn "Day $1A: %s" (part1 input)
//    printfn "Day $1B: %s" (part2 input)
    0
EOF
