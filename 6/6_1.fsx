open System.IO

type Planet = Planet of string

let centreOfMass = Planet("COM")

type Orbit =
    { Parent: Planet
      Child: Planet }

let parseOrbit (x: string): Orbit =
    let planets = x.Split(')')
    { Parent = Planet(planets.[0])
      Child = Planet(planets.[1]) }

type PlanetTree =
    | Leaf of Planet
    | Branch of Planet * PlanetTree list

let rec getNode (orbits: Map<Planet, Planet list>) (planet: Planet): PlanetTree =
    match Map.tryFind planet orbits with
    | Some(children) -> Branch(planet, children |> List.map (getNode orbits))
    | None -> Leaf(planet)

let buildTree (orbits: Orbit list): PlanetTree =
    let orbitsByParent =
        orbits
        |> Seq.groupBy (fun orbit -> orbit.Parent)
        |> Seq.map (fun (parent, orbits) ->
            (parent,
             orbits
             |> Seq.map (fun orbit -> orbit.Child)
             |> Seq.toList))
        |> Map.ofSeq

    getNode orbitsByParent centreOfMass

let rec countOrbits (tree: PlanetTree) (currentOrbits: int): int =
    let currentPlanetOrbits = currentOrbits + 1
    match tree with
    | Leaf(planet) -> currentPlanetOrbits
    | Branch(planet, children) ->
        children |> List.fold (fun acc child -> acc + countOrbits child currentPlanetOrbits) currentPlanetOrbits

let tree =
    File.ReadLines("./6_input.txt")
    |> Seq.map parseOrbit
    |> Seq.toList
    |> buildTree

let totalOrbits = countOrbits tree -1

printf "Part 1 - total number of orbits: %d" totalOrbits
