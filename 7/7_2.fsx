#load "./IntCode.fs"

open System.IO
open IntCode

let program = File.ReadAllText("./7_input.txt") |> parseIntCodeProgram

let runAmplifiers (phaseSettings: int list) =
    let amplifiers =
        phaseSettings |> List.map (fun phaseSetting -> initialiseIntCodeComputer program [| phaseSetting |])

    let rec runCycle (amplifiers: IntCodeComputer list) (input: int option): IntCodeComputer list * int option =
        let foldAmplifier (amplifiers: IntCodeComputer list, input: int option) (amplifier: IntCodeComputer): IntCodeComputer list * int option =
            let ampWithInput =
                match input with
                | Some input -> { amplifier with inputs = (Seq.append amplifier.inputs [ input ]) }
                | None -> amplifier

            let (newState, output) = runIntCodeComputer ampWithInput
            (List.append amplifiers [ newState ], output)

        let (newState, output) = amplifiers |> List.fold foldAmplifier ([], input)

        match output with
        | Some _ -> runCycle newState output
        | None -> (newState, output)

    let (finalState, _) = runCycle amplifiers (Some 0)

    let finalAmp =
        finalState
        |> List.rev
        |> List.head
    finalAmp.outputs
    |> Seq.rev
    |> Seq.head

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

let phaseSettings = permutations (seq { 5 .. 9 } |> Seq.toList)

let bestSignal =
    phaseSettings
    |> Seq.map (fun settings -> (settings, runAmplifiers settings))
    |> Seq.maxBy snd

printf "Best signal: %d from phase setting %A" (snd bestSignal) (fst bestSignal)
