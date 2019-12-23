module IntCode

let debug = false

type ParamMode = | Immediate | Position

let private getParamMode x =
    match x with
    | 0 -> Position
    | 1 -> Immediate
    | _ -> invalidArg "x" ("Unrecognised paramMode " + (string x))

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

let private getDigits (x: string): int seq =
    x |> Seq.map (fun c -> int c - int '0')

let private parseOpCode x: OpCode =
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
    | _ -> failwith ("Invalid opCode " + string x)


type Program = Map<int, int>

type IntCodeState = {
    program: Program
    position: int
    inputs: int seq
}

let parseIntCodeProgram (program: string): Program =
    program.Split(',') |> Seq.map int |> Seq.indexed |> Map.ofSeq

let private getParam (state: IntCodeState) (offset: int) (mode: ParamMode): int =
    let { program = program; position = position; } = state
    let index = position + offset
    match mode with
    | Position -> state.program.[program.[index]]
    | Immediate -> state.program.[index]

let private updateProgram (state: IntCodeState) (outputParamOffset: int) (newValue: int): Program =
    let outputIndex = state.program.[state.position + outputParamOffset]
    if debug then printf "-- Updating %d to %d\r\n" outputIndex newValue
    state.program |> Map.add outputIndex newValue

let rec runIntCodeComputer (state: IntCodeState): state: IntCodeState * output: int option =
    let { program = program; position = position; inputs = inputs } = state

    if debug then printf "\r\nIndex: %d\tOpcode: %d \t%A\tArguments: %A\r\n" position program.[position] (parseOpCode program.[position]) (program.[position + 1], program.[position + 2], program.[position + 3])

    match parseOpCode program.[position] with
    | Add(xMode, yMode) ->
        let x = getParam state 1 xMode
        let y = getParam state 2 yMode
        runIntCodeComputer
            { state with
                program = updateProgram state 3 (x + y)
                position = position + 4 }

    | Multiply(xMode, yMode) ->
        let x = getParam state 1 xMode
        let y = getParam state 2 yMode
        runIntCodeComputer
            { state with
                program = updateProgram state 3 (x * y)
                position = position + 4 }

    | Input ->
        runIntCodeComputer
            { state with
                program = updateProgram state 1 (Seq.head inputs)
                position = position + 2
                inputs = Seq.tail inputs }

    | Output(paramMode) ->
        let outputValue = getParam state 1 paramMode
        ({ state with position = position + 2 }, Some outputValue)

    | JumpIfTrue(xMode, yMode) ->
        let value = getParam state 1 xMode
        let jumpTo = getParam state 2 yMode
        runIntCodeComputer
            { state with
                position = (if value <> 0 then jumpTo else position + 3) }

    | JumpIfFalse(xMode, yMode) ->
        let value = getParam state 1 xMode
        let jumpTo = getParam state 2 yMode
        runIntCodeComputer
            { state with
                position = (if value = 0 then jumpTo else position + 3) }

    | LessThan(xMode, yMode) ->
        let x = getParam state 1 xMode
        let y = getParam state 2 yMode
        runIntCodeComputer
            { state with
                program = updateProgram state 3 (if x < y then 1 else 0)
                position = position + 4 }

    | Equals(xMode, yMode) ->
        let x = getParam state 1 xMode
        let y = getParam state 2 yMode
        runIntCodeComputer
            { state with
                program = updateProgram state 3 (if x = y then 1 else 0)
                position = position + 4 }

    | Halt -> (state, None)
