open System.IO

let asteroids =
    File.ReadLines("./input.txt")
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x cell -> (cell, x, y)))
    |> Seq.reduce Seq.append
    |> Seq.filter (fun (cell, _, _) -> cell = '#')
    |> Seq.map (fun (_, x, y) -> (x, y))
    |> List.ofSeq

let station = (17, 23)
let stationX = fst station
let stationY = snd station

let isBetween (x1, y1) (x2, y2) (x3, y3): bool =
    (x1, y1) <> (x3, y3) && (x2, y2) <> (x3, y3) && (x1 <= x3 && x3 <= x2 || x1 >= x3 && x3 >= x2)
    && (y1 <= y3 && y3 <= y2 || y1 >= y3 && y3 >= y2)

let isInLineOfSight asteroids (x1, y1) (x2, y2): bool =
    asteroids
    |> Seq.filter (isBetween (x1, y1) (x2, y2))
    |> Seq.filter (fun (x3, y3) -> (y2 - y1) * (x3 - x1) = (y3 - y1) * (x2 - x1))
    |> Seq.isEmpty

let getOrdering (x, y): int * float =
    let gradient = float (y - stationY) / float (x - stationX)
    match x >= stationX with
    | true ->
        match y > stationY with
        | true -> (1, gradient) // Bottom right
        | false -> (0, gradient) // Top right
    | false ->
        match y >= stationY with
        | true -> (2, gradient) // Bottom left
        | false -> (3, gradient) // Top left

let runRotation (asteroids: (int * int) list): (int * int) list * (int * int) list =
    let (destroyed, remaining) = asteroids |> List.partition (isInLineOfSight asteroids station)
    (remaining, destroyed |> List.sortBy getOrdering)

let runAllRotations =
    let mutable destroyedAsteroids = Seq.empty
    let mutable remainingAsteroids = asteroids |> List.except [ station ]

    while (not (List.isEmpty remainingAsteroids)) do
        let (remaining, destroyed) = runRotation remainingAsteroids
        remainingAsteroids <- remaining
        destroyedAsteroids <- Seq.append destroyedAsteroids destroyed

    destroyedAsteroids |> List.ofSeq

printf "%A" (runAllRotations).[199]
