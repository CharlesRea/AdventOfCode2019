type Vector =
    { x: int
      y: int
      z: int }
    static member (+) (v1: Vector, v2: Vector) =
        { x = v1.x + v2.x
          y = v1.y + v2.y
          z = v1.z + v2.z }

    override m.ToString() =
        sprintf "(%d, %d, %d)" m.x m.y m.z

let vectorSize (vector: Vector): int =
    abs vector.x + abs vector.y + abs vector.z

type Planet =
    { position: Vector
      velocity: Vector }
    override m.ToString() =
        sprintf "pos=%O, vel=%O" m.position m.velocity

let createPlanet (x, y, z) =
    { position =
          { x = x
            y = y
            z = z }
      velocity =
          { x = 0
            y = 0
            z = 0 } }

let energy (planet: Planet) = vectorSize planet.position * vectorSize planet.velocity

let applyGravityToPlanet (planets: Planet list) (planet: Planet): Planet =
    let applySinglePlanetGravity (planet: Planet) (otherPlanet: Planet): Planet =
        let calculateScalarVelocity (getScalar: Vector -> int) =
            match getScalar otherPlanet.position - getScalar planet.position with
            | difference when difference > 0 -> getScalar planet.velocity + 1
            | difference when difference < 0 -> getScalar planet.velocity - 1
            | _ -> getScalar planet.velocity

        { planet with
              velocity =
                  { x = calculateScalarVelocity (fun vector -> vector.x)
                    y = calculateScalarVelocity (fun vector -> vector.y)
                    z = calculateScalarVelocity (fun vector -> vector.z) } }

    planets |> List.fold applySinglePlanetGravity planet

let applyGravity (planets: Planet list) =
    planets |> List.map (applyGravityToPlanet planets)

let applyVelocity (planets: Planet list) =
    let singlePlanetVelocity (planet: Planet) =
        { planet with position = planet.position + planet.velocity }
    planets |> List.map singlePlanetVelocity

let runTimeStep (planets: Planet list): Planet list =
    planets
    |> applyGravity
    |> applyVelocity

let runTimeSteps (planets: Planet list) (count: int): Planet list =
    seq { 0 .. count - 1 } |> Seq.fold (fun planets _ -> runTimeStep planets) planets


let initialPlanets =
    [ createPlanet (8, 0, 8)
      createPlanet (0, -5, -10)
      createPlanet (16, 10, -5)
      createPlanet (19, -10, -7) ]

let finalPlanets = runTimeSteps initialPlanets 1000

for planet in finalPlanets do
    printf "%O\r\n" planet

let totalEnergy = List.sumBy energy finalPlanets
printf "%d" totalEnergy