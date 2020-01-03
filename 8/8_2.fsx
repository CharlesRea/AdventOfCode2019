open System.IO

let width = 25
let height = 6
let layerSize = width * height

let digits (x: string) =
    x |> Seq.map (fun c -> int c - int '0')

let data = File.ReadAllText("./8_input.txt") |> digits

let layers = Seq.chunkBySize layerSize data

type Colour =
    | Black
    | White
    | Transparent

let getColour pixel =
    match pixel with
    | 0 -> Black
    | 1 -> White
    | 2 -> Transparent
    | _ -> failwith "unrecognised pixel"

let getDisplayedColour x y =
    layers
    |> Seq.map (fun layer -> layer.[x + y * width])
    |> Seq.map getColour
    |> Seq.tryFind (fun colour -> colour <> Transparent)
    |> Option.defaultValue Transparent

let formatColour colour =
    match colour with
    | Black -> "."
    | White -> "#"
    | Transparent -> " "

for y in 0 .. height - 1 do
    let row =
        seq { 0 .. width - 1 }
        |> Seq.map (fun x -> getDisplayedColour x y)
        |> Seq.map formatColour
        |> String.concat ""
    printf "%s\r\n" row
