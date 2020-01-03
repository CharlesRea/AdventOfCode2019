#load "./IntCode.fs"

open System.IO
open IntCode

let program = File.ReadAllText("./7_input.txt") |> parseIntCodeProgram

let runAmplifier (input: int) (phaseSetting: int): int =
    let inputs = [| phaseSetting; input |]
    let (_, output) = runIntCodeComputer (initialiseIntCodeComputer program inputs)
    match output with
    | Some(output) -> output
    | None -> failwith "No output from program"

let runAmplifiers (phaseSettings: int list) =
    List.fold runAmplifier 0 phaseSettings

let rec permutations (xs: 'a list) =
    seq {
        if (xs.IsEmpty) then
            yield []
        else

            for i in xs do
                yield! xs
                       |> List.filter (fun x -> x <> i)
                       |> permutations
                       |> Seq.map (fun x -> i :: x)
    }

let phaseSettings = permutations (seq { 0 .. 4 } |> Seq.toList)

let bestSignal =
    phaseSettings
    |> Seq.map (fun settings -> (settings, runAmplifiers settings))
    |> Seq.maxBy snd

printf "Best signal: %d from phase setting %A" (snd bestSignal) (fst bestSignal)
