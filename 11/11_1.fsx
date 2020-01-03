#load "./IntCode.fs"

open System.IO
open IntCode

let program = File.ReadAllText("./input.txt") |> parseIntCodeProgram

let paintedCoordinates = Map.empty

type Direction =
    | Up
    | Right
    | Down
    | Left

type RotationDirection =
    | TurnLeft
    | TurnRight

type Colour =
    | Black
    | White

type Robot =
    { direction: Direction
      position: int * int
      paintedCoordinates: Map<int * int, Colour> }

let currentColour (robot: Robot): Colour =
    robot.paintedCoordinates
    |> Map.tryFind robot.position
    |> Option.defaultValue Black

let colourToInput (colour: Colour) =
    match colour with
    | White -> 1L
    | Black -> 0L

let outputToColour (value: int64) =
    match value with
    | 0L -> Black
    | 1L -> White
    | _ -> failwith (sprintf "Unrecognised colour output value: %d" value)

let outputToDirection (value: int64) (direction: Direction): Direction =
    let turnDirection =
        match value with
        | 0L -> TurnLeft
        | 1L -> TurnRight
        | _ -> failwith (sprintf "Unrecognised direction output value: %d" value)

    match (direction, turnDirection) with
    | (Up, TurnLeft) -> Left
    | (Up, TurnRight) -> Right
    | (Right, TurnLeft) -> Up
    | (Right, TurnRight) -> Down
    | (Down, TurnLeft) -> Right
    | (Down, TurnRight) -> Left
    | (Left, TurnLeft) -> Down
    | (Left, TurnRight) -> Up

let move (x, y) (direction: Direction) =
    match direction with
    | Up -> (x, y + 1)
    | Right -> (x + 1, y)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)


let runRobot: Robot =
    let rec runStep (robot: Robot) (program: IntCodeComputer): Robot =
        let inputValue =
            robot
            |> currentColour
            |> colourToInput

        let programWithInputs = { program with inputs = Seq.initInfinite (fun _ -> inputValue) }

        let (newProgram, colourOutput) = runIntCodeComputer programWithInputs

        match colourOutput with
        | None -> robot

        | Some colourOutput ->
            let colourToPaint = outputToColour colourOutput
            let paintedCoordinates = robot.paintedCoordinates |> Map.add robot.position colourToPaint
            let (finalProgram, directionOutput) = runIntCodeComputer newProgram

            match directionOutput with
            | None -> { robot with paintedCoordinates = paintedCoordinates }

            | Some directionOutput ->
                let direction = outputToDirection directionOutput robot.direction

                let newRobot =
                    { paintedCoordinates = robot.paintedCoordinates |> Map.add robot.position colourToPaint
                      direction = direction
                      position = move robot.position direction }
                runStep newRobot finalProgram

    let initialComputer = initialiseIntCodeComputer program Seq.empty

    let initialRobot =
        { paintedCoordinates = Map.empty
          direction = Up
          position = (0, 0) }
    runStep initialRobot initialComputer

let robot = runRobot

printf "Painted squares: %d" (Map.count robot.paintedCoordinates)
