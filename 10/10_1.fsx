open System.IO

let asteroids =
    File.ReadLines("./input.txt")
    |> Seq.mapi (fun y row -> row |> Seq.mapi (fun x cell -> (cell, x, y)))
    |> Seq.reduce Seq.append
    |> Seq.filter (fun (cell, _, _) -> cell = '#')
    |> Seq.map (fun (_, x, y) -> (x, y))
    |> List.ofSeq

let isBetween (x1, y1) (x2, y2) (x3, y3): bool =
    (x1, y1) <> (x3, y3) && (x2, y2) <> (x3, y3) && (x1 <= x3 && x3 <= x2 || x1 >= x3 && x3 >= x2)
    && (y1 <= y3 && y3 <= y2 || y1 >= y3 && y3 >= y2)

let isInLineOfSight (x1, y1) (x2, y2): bool =
    asteroids
    |> Seq.filter (isBetween (x1, y1) (x2, y2))
    |> Seq.filter (fun (x3, y3) -> (y2 - y1) * (x3 - x1) = (y3 - y1) * (x2 - x1))
    |> Seq.isEmpty

let countInLineOfSight asteroid =
    asteroids
    |> Seq.except [ asteroid ]
    |> Seq.filter (isInLineOfSight asteroid)
    |> Seq.length

let maxInSight =
    asteroids
    |> Seq.map (fun asteroid -> (asteroid, countInLineOfSight asteroid))
    |> Seq.maxBy snd

printf "%A" maxInSight
