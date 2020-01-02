module IntCode

let debug = false

type ParamMode = | Immediate | Position | Relative

let private getParamMode x =
    match x with
    | 0 -> Position
    | 1 -> Immediate
    | 2 -> Relative
    | _ -> invalidArg "x" ("Unrecognised paramMode " + (string x))

type OpCode =
    | Add of xMode: ParamMode * yMode: ParamMode * outputMode: ParamMode
    | Multiply of xMode: ParamMode * yMode: ParamMode * outputMode: ParamMode
    | Input of outputMode: ParamMode
    | Output of inputMode: ParamMode
    | JumpIfTrue of xMode: ParamMode * yMode: ParamMode
    | JumpIfFalse of xMode: ParamMode * yMode: ParamMode
    | LessThan of xMode: ParamMode * yMode: ParamMode * outputMode: ParamMode
    | Equals of xMode: ParamMode * yMode: ParamMode * outputMode: ParamMode
    | AdjustRelativeBase of mode: ParamMode
    | Halt

let private getDigits (x: string): int seq =
    x |> Seq.map (fun c -> int c - int '0')

let private parseOpCode x: OpCode =
    let digits = (string x).PadLeft(5, '0') |> getDigits |> Seq.toList |> List.rev

    match digits with
    | 1 :: 0 :: x :: y :: z :: [] -> Add(getParamMode x, getParamMode y, getParamMode z)
    | 2 :: 0 :: x :: y :: z :: [] -> Multiply(getParamMode x, getParamMode y, getParamMode z)
    | 3 :: 0 :: x :: 0 :: 0 :: [] -> Input(getParamMode x)
    | 4 :: 0 :: x :: 0 :: 0 :: [] -> Output(getParamMode x)
    | 5 :: 0 :: x :: y :: 0 :: [] -> JumpIfTrue(getParamMode x, getParamMode y)
    | 6 :: 0 :: x :: y :: 0 :: [] -> JumpIfFalse(getParamMode x, getParamMode y)
    | 7 :: 0 :: x :: y :: z :: [] -> LessThan(getParamMode x, getParamMode y, getParamMode z)
    | 8 :: 0 :: x :: y :: z :: [] -> Equals(getParamMode x, getParamMode y, getParamMode z)
    | 9 :: 0 :: x :: 0 :: 0 :: [] -> AdjustRelativeBase(getParamMode x)
    | 9 :: 9 :: _ -> Halt
    | _ -> failwith ("Invalid opCode " + string x)


type Program = Map<int64, int64>

type IntCodeComputer = {
    program: Program
    position: int64
    relativeBase: int64
    inputs: int64 seq
    outputs: int64 seq
}

let initialiseIntCodeComputer (program: Program) (inputs: int64 seq): IntCodeComputer =
    { program = program; position = 0L; inputs = inputs; outputs = []; relativeBase = 0L }

let parseIntCodeProgram (program: string): Program =
    program.Split(',') |> Seq.map int64 |> Seq.indexed |> Seq.map (fun (index, value) -> (int64 index, value)) |> Map.ofSeq

let private getValue (state: IntCodeComputer) (index: int64): int64 =
    match Map.tryFind index state.program with
    | Some value -> value
    | None -> int64 0

let private getParamValue (state: IntCodeComputer) (offset: int) (mode: ParamMode): int64 =
    let param = getValue state (state.position + int64 offset)
    match mode with
    | Position -> getValue state param
    | Immediate -> param
    | Relative -> getValue state (state.relativeBase + param)

let private getParamIndex (state: IntCodeComputer) (offset: int) (mode: ParamMode): int64 =
    let param = getValue state (state.position + int64 offset)
    match mode with
    | Position -> param
    | Immediate -> failwith "Cannot get index of immediate param"
    | Relative -> state.relativeBase + param

let private updateProgram (state: IntCodeComputer) (outputIndex: int64) (newValue: int64): Program =
    if debug then printf "-- Updating %d to %d\r\n" outputIndex newValue
    state.program |> Map.add outputIndex newValue

let rec runIntCodeComputer (state: IntCodeComputer): IntCodeComputer * int64 option =
    let { position = position; inputs = inputs } = state

    if debug then printf "\r\nOpcode: %d\tPosition: %d\t%A\tArguments: %A\r\n" (getValue state position) position (parseOpCode (getValue state position)) (getValue state (position + 1L), getValue state (position + 2L), getValue state (position + 3L))

    match parseOpCode (getValue state position) with
    | Add(xMode, yMode, outputMode) ->
        let x = getParamValue state 1 xMode
        let y = getParamValue state 2 yMode
        let outputIndex = getParamIndex state 3 outputMode
        runIntCodeComputer
            { state with
                program = updateProgram state outputIndex (x + y)
                position = position + 4L }

    | Multiply(xMode, yMode, outputMode) ->
        let x = getParamValue state 1 xMode
        let y = getParamValue state 2 yMode
        let outputIndex = getParamIndex state 3 outputMode
        runIntCodeComputer
            { state with
                program = updateProgram state outputIndex (x * y)
                position = position + 4L }

    | Input(outputMode) ->
        let outputIndex = getParamIndex state 1 outputMode
        runIntCodeComputer
            { state with
                program = updateProgram state outputIndex (Seq.head inputs)
                position = position + 2L
                inputs = Seq.tail inputs }

    | Output(paramMode) ->
        let outputValue = getParamValue state 1 paramMode
        let finalState =
            { state with
                outputs = Seq.append state.outputs [outputValue]
                position = position + 2L }
        (finalState, Some outputValue)

    | JumpIfTrue(xMode, yMode) ->
        let value = getParamValue state 1 xMode
        let jumpTo = getParamValue state 2 yMode
        runIntCodeComputer
            { state with
                position = (if value <> int64 0 then jumpTo else position + 3L) }

    | JumpIfFalse(xMode, yMode) ->
        let value = getParamValue state 1 xMode
        let jumpTo = getParamValue state 2 yMode
        runIntCodeComputer
            { state with
                position = (if value = 0L then jumpTo else position + 3L) }

    | LessThan(xMode, yMode, outputMode) ->
        let x = getParamValue state 1 xMode
        let y = getParamValue state 2 yMode
        let outputIndex = getParamIndex state 3 outputMode
        runIntCodeComputer
            { state with
                program = updateProgram state outputIndex (if x < y then 1L else 0L)
                position = position + 4L }

    | Equals(xMode, yMode, outputMode) ->
        let x = getParamValue state 1 xMode
        let y = getParamValue state 2 yMode
        let outputIndex = getParamIndex state 3 outputMode
        runIntCodeComputer
            { state with
                program = updateProgram state outputIndex (if x = y then 1L else 0L)
                position = position + 4L }

    | AdjustRelativeBase (paramMode) ->
        let offset = getParamValue state 1 paramMode
        runIntCodeComputer
            { state with
                relativeBase = state.relativeBase + offset
                position = position + 2L }

    | Halt -> (state, None)
