open System.IO

let rec getFuel mass =
    let fuel = max (mass / 3 - 2) 0
    if fuel = 0 then 0 else fuel + getFuel fuel

let getTotalFuel =
    File.ReadLines("./1_input.txt")
        |> Seq.map int
        |> Seq.map getFuel
        |> Seq.sum

printf "%d" getTotalFuel
