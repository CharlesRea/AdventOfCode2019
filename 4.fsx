let toDigitArray x =
    x |> string |> Seq.map (fun c -> int c - int '0')

let hasRepeatingAdjacentDigits (digits: seq<int>) =
    digits |> Seq.pairwise |> Seq.exists (fun (x, y) -> x = y)

// Returns all quadruples of adjacent elements - ie (1,2,3,4), (2,3,4,5), etc
let quadwise (xs: 'a seq): ('a * 'a * 'a * 'a) seq =
    let mutable first = None
    let mutable second = None
    let mutable third = None
    let mutable fourth = None
    seq {
        for x in xs do
            first <- second
            second <- third
            third <- fourth
            fourth <- Some(x)
            match (first, second, third, fourth) with
            | (Some(first), Some(second), Some(third), Some(fourth)) -> yield (first, second, third, fourth)
            | _ -> ()
    }

let hasExactlyTwoRepeatingAdjacentDigits (digits: seq<int>) =
    let paddedDigits = [None] |> Seq.append (Seq.map Some digits) |> Seq.append [None]
    quadwise paddedDigits |> Seq.exists (fun (w, x, y, z) -> w <> x && x = y && y <> z)

let isIncreasing (digits: seq<int>) =
    digits |> Seq.pairwise |> Seq.forall (fun (x, y) -> x <= y)

let isValidPassword password =
    let digits = toDigitArray password
    Seq.length digits = 6 && hasExactlyTwoRepeatingAdjacentDigits digits && isIncreasing digits

let passwords = seq { 165432 .. 707912 }
let numberValidPasswords = passwords |> Seq.filter isValidPassword |> Seq.length

printf "%d" numberValidPasswords
