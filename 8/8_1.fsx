open System.IO

let width = 25
let height = 6
let layerSize = width * height

let digits (x: string) =
    x |> Seq.map (fun c -> int c - int '0')

let data = File.ReadAllText("./8_input.txt") |> digits

let layers = Seq.chunkBySize layerSize data

let digitFrequency digit layer =
    layer |> Seq.filter (fun x -> x = digit) |> Seq.length

let layerWithFewestZeroes = layers |> Seq.minBy (digitFrequency 0)

let part1Answer = (digitFrequency 1 layerWithFewestZeroes) * (digitFrequency 2 layerWithFewestZeroes)
printf "Part 1: %d\r\n" part1Answer
