open System.IO

let initialProgram = File.ReadAllText("./7_input.txt").Split(',') |> Seq.map int |> Seq.toArray

type ParamMode = | Immediate | Position

type OpCode =
    | Add of xMode: ParamMode * yMode: ParamMode
    | Multiply of xMode: ParamMode * yMode: ParamMode
    | Input
    | Output of inputMode: ParamMode
    | JumpIfTrue of xMode: ParamMode * yMode: ParamMode
    | JumpIfFalse of xMode: ParamMode * yMode: ParamMode
    | LessThan of xMode: ParamMode * yMode: ParamMode
    | Equals of xMode: ParamMode * yMode: ParamMode
    | Halt

let getDigits x =
    x |> string |> Seq.map (fun c -> int c - int '0')

let getParamMode x =
    match x with
    | 0 -> Position
    | 1 -> Immediate
    | _ -> invalidArg "x" ("Unrecognised paramMode " + (string x))

let getParamModes xs =
    xs |> List.map getParamMode

let parseOpCode x: OpCode =
    let digits = (string x).PadLeft(5, '0') |> getDigits |> Seq.toList |> List.rev

    match digits with
    | 1 :: 0 :: x :: y :: 0 :: [] -> Add(getParamMode x, getParamMode y)
    | 2 :: 0 :: x :: y :: 0 :: [] -> Multiply(getParamMode x, getParamMode y)
    | 3 :: 0 :: 0 :: 0 :: 0 :: [] -> Input
    | 4 :: 0 :: x :: 0 :: 0 :: [] -> Output(getParamMode x)
    | 5 :: 0 :: x :: y :: 0 :: [] -> JumpIfTrue(getParamMode x, getParamMode y)
    | 6 :: 0 :: x :: y :: 0 :: [] -> JumpIfFalse(getParamMode x, getParamMode y)
    | 7 :: 0 :: x :: y :: 0 :: [] -> LessThan(getParamMode x, getParamMode y)
    | 8 :: 0 :: x :: y :: 0 :: [] -> Equals(getParamMode x, getParamMode y)
    | 9 :: 9 :: _ -> Halt
    | _ -> failwith ("Invalid opCode " + x.ToString())

let getParamValue (program: array<int>) inputValue (mode: ParamMode) =
    match mode with
    | Position -> program.[inputValue]
    | Immediate -> inputValue

type Host = {
    inputs: int []
    output: int option
 }

let rec runStep (program: array<int>) (host: Host) (index: int): Host =
    let opCode = parseOpCode program.[index]

    match opCode with
    | Add(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (x + y)
        runStep program host (index + 4)
    | Multiply(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (x * y)
        runStep program host (index + 4)
    | Input ->
        let input = Array.head host.inputs
        Array.set program program.[index + 1] input
        let newHost = { host with inputs = Array.tail host.inputs }
        runStep program newHost (index + 2)
    | Output(paramMode) ->
        let outputValue = getParamValue program program.[index + 1] paramMode
        let newHost = { host with output = Some(outputValue) }
        runStep program newHost (index + 2)
    | JumpIfTrue(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        runStep program host (if x <> 0 then y else index + 3)
    | JumpIfFalse(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        runStep program host (if x = 0 then y else index + 3)
    | LessThan(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (if x < y then 1 else 0)
        runStep program host (index + 4)
    | Equals(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (if x = y then 1 else 0)
        runStep program host (index + 4)
    | Halt -> host


let runAmplifier  (input: int) (phaseSetting: int) : int =
    let host = { inputs = [| phaseSetting; input; |]; output = None }
    let finalHost = runStep initialProgram host 0
    match finalHost.output with
    | Some(output) -> output
    | None -> failwith "No output from program"

let runAmplifiers (phaseSettings: int list) =
    List.fold runAmplifier 0 phaseSettings

let rec permutations (xs: 'a list) = seq {
    if (xs.IsEmpty) then
        yield []
    else

            for i in xs do
            yield! xs
                    |> List.filter (fun x -> x <> i)
                    |> permutations
                    |> Seq.map (fun x -> i :: x)
    }

let phaseSettings = permutations (seq { 0..4 } |> Seq.toList)

let bestSignal = phaseSettings
                 |> Seq.map (fun settings -> (settings, runAmplifiers settings))
                 |> Seq.maxBy snd

printf "Best signal: %d from phase setting %A" (snd bestSignal) (fst bestSignal)
