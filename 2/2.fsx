open System.IO

let initialProgram =
    File.ReadAllText("./2_input.txt").Split(',')
    |> Seq.map int
    |> Seq.toArray

let rec runStep (program: array<int>, currentIndex: int) =
    match program.[currentIndex] with
    | 1 ->
        Array.set program program.[currentIndex + 3]
            (program.[program.[currentIndex + 1]] + program.[program.[currentIndex + 2]])
        let newIndex = currentIndex + 4
        runStep (program, newIndex)
    | 2 ->
        Array.set program program.[currentIndex + 3]
            (program.[program.[currentIndex + 1]] * program.[program.[currentIndex + 2]])
        let newIndex = currentIndex + 4
        runStep (program, newIndex)
    | _ -> program.[0]

let run x y =
    let program = Array.copy initialProgram
    program.[1] <- x
    program.[2] <- y
    runStep (program, 0)

let answer = Seq.allPairs { 0 .. 99 } { 0 .. 99 } |> Seq.tryFind (fun (x, y) -> run x y = 19690720)

printf "%A" answer
