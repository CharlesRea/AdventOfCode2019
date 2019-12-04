open System
open System.IO

let (|PathDirection|_|) (direction:string) (path:string) =
    if path.StartsWith(direction) then
        Some(path.Substring(direction.Length) |> int)
    else
        None

let getPathCoordinates (x, y) (path: string) =
    match path with
    | PathDirection "U" length -> seq { for i in 1 .. length -> (x + i, y) }
    | PathDirection "R" length -> seq { for i in 1 .. length -> (x, y + i) }
    | PathDirection "D" length -> seq { for i in 1 .. length -> (x - i, y) }
    | PathDirection "L" length -> seq { for i in 1 .. length -> (x, y - i) }
    | _ -> raise (ArgumentException "Unexpected path")

let getWireCoordinates (wire: seq<string>) =
    let initialPath = [(0,0)] |> seq
    wire |> Seq.fold (fun currentPath path -> Seq.append currentPath (getPathCoordinates (Seq.last currentPath) path)) initialPath

let intersect (xs:'a seq) (ys: 'a seq) = Set.intersect (xs |> set) (ys |> set)

let input = File.ReadLines("./3_input.txt") |> Seq.map (fun x -> x.Split(','))

let wire1 = getWireCoordinates (Seq.head input)
let wire2 = getWireCoordinates (Seq.last input)

let intersections = intersect (Seq.tail wire1) (Seq.tail wire2)

let distanceToPoint wire point =
    wire |> Seq.findIndex (fun x -> x = point)

let intersectionDistances = intersections |> Seq.map (fun point -> distanceToPoint wire1 point + distanceToPoint wire2 point)

let minDistance = intersectionDistances |> Seq.min

printf "%d" minDistance
