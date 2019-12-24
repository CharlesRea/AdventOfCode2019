#load "./IntCode.fs"

open System.IO
open IntCode

let program = File.ReadAllText("./input.txt") |> parseIntCodeProgram

let finalState = runIntCodeComputer (initialiseIntCodeComputer program [| 1L; |])

printf "Outputs: %A" finalState.outputs
