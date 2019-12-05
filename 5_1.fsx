open System.IO

let initialProgram = File.ReadAllText("./5_input.txt").Split(',') |> Seq.map int |> Seq.toArray

type ParamMode = | Immediate | Position

type OpCode =
    | Add of xMode: ParamMode * yMode: ParamMode
    | Multiply of xMode: ParamMode * yMode: ParamMode
    | Input
    | Output of inputMode: ParamMode
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
    | 9 :: 9 :: _ -> Halt
    | _ -> failwith "Invalid opCode"


let getParamValue (program: array<int>) inputValue (mode: ParamMode) =
    match mode with
    | Position -> program.[inputValue]
    | Immediate -> inputValue

let getInput =
    1

let output x =
    printf "Output: %d\r\n" x

let rec runStep (program: array<int>) (index: int) =
    let opCode = parseOpCode program.[index]

    match opCode with
    | Add(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (x + y)
        runStep program (index + 4)
    | Multiply(xMode, yMode) ->
        let x = getParamValue program program.[index + 1] xMode
        let y = getParamValue program program.[index + 2] yMode
        Array.set program program.[index + 3] (x * y)
        runStep program (index + 4)
    | Input ->
        Array.set program program.[index + 1] getInput
        runStep program (index + 2)
    | Output(paramMode) ->
        let outputValue = getParamValue program program.[index + 1] paramMode
        output outputValue
        runStep program (index + 2)
    | Halt -> ()

runStep initialProgram 0