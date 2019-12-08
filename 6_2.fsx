open System.IO
open System.Linq

type Planet = Planet of string

let centreOfMass = Planet("COM")
let you = Planet("YOU")
let santa = Planet("SAN")

type Orbit = {
    Parent: Planet
    Child: Planet
 }

let parseOrbit (x: string): Orbit =
    let planets = x.Split(')')
    { Parent = Planet(planets.[0]); Child = Planet(planets.[1]) }

type PlanetTree =
    | Leaf of Planet
    | Branch of Planet * PlanetTree list

let rec getNode (orbits: Map<Planet, Planet list>) (planet: Planet): PlanetTree =
    match Map.tryFind planet orbits with
    | Some(children) -> Branch(planet, children |> List.map (getNode orbits))
    | None -> Leaf(planet)

let buildTree (orbits: Orbit list): PlanetTree =
    let orbitsByParent = orbits
                         |> Seq.groupBy (fun orbit -> orbit.Parent)
                         |> Seq.map (fun (parent, orbits) -> (parent, orbits |> Seq.map (fun orbit -> orbit.Child) |> Seq.toList))
                         |> Map.ofSeq

    getNode orbitsByParent centreOfMass

let rec countOrbits (tree: PlanetTree) (currentOrbits: int) : int =
    let currentPlanetOrbits = currentOrbits + 1
    match tree with
    | Leaf(planet) -> currentPlanetOrbits
    | Branch(planet, children) ->
        children |> List.fold (fun acc child -> acc + countOrbits child currentPlanetOrbits) currentPlanetOrbits

let orbits = File.ReadLines("./6_input.txt") |> Seq.map parseOrbit |> Seq.toList
let tree = buildTree orbits

let totalOrbits = countOrbits tree -1
printf "Part 1 - total number of orbits: %d" totalOrbits

let rec getAncestors (planet: Planet) (parentsByChildren: Map<Planet, Planet>): Planet seq =
    match Map.tryFind planet parentsByChildren with
    | Some(parent) -> Seq.append [parent] (getAncestors parent parentsByChildren)
    | None -> [planet] |> List.toSeq

let getMinimumDistance (x: Planet) (y: Planet) (orbits: Orbit list) =
    let parentsByChildren = orbits |> Seq.map (fun orbit -> (orbit.Child, orbit.Parent)) |> Map.ofSeq

    let xAncestors = getAncestors x parentsByChildren
    let yAncestors = getAncestors y parentsByChildren

    let firstAncestor = xAncestors.Intersect yAncestors |> Seq.head

    Seq.findIndex (fun ancestor -> ancestor = firstAncestor) xAncestors + Seq.findIndex (fun ancestor -> ancestor = firstAncestor) yAncestors

printf "Part 2 - Orbit transfers required %d" (getMinimumDistance you santa orbits)
