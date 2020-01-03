type Vector =
    { x: int
      y: int
      z: int }
    static member (+) (v1: Vector, v2: Vector) =
        { x = v1.x + v2.x
          y = v1.y + v2.y
          z = v1.z + v2.z }

type Planet =
    { position: Vector
      velocity: Vector }

let createPlanet (x, y, z) =
    { position =
          { x = x
            y = y
            z = z }
      velocity =
          { x = 0
            y = 0
            z = 0 } }

type PlanetScalar =
    { position: int
      velocity: int }

let toScalar (mapper: Vector -> int) (planet: Planet) =
    { PlanetScalar.position = mapper planet.position
      PlanetScalar.velocity = mapper planet.velocity }

let applyGravityToPlanet (planets: PlanetScalar list) (planet: PlanetScalar): PlanetScalar =
    let applySinglePlanetGravity (planet: PlanetScalar) (otherPlanet: PlanetScalar): PlanetScalar =
        let velocity =
            match otherPlanet.position - planet.position with
            | difference when difference > 0 -> planet.velocity + 1
            | difference when difference < 0 -> planet.velocity - 1
            | _ -> planet.velocity

        { planet with velocity = velocity }

    planets |> List.fold applySinglePlanetGravity planet

let applyGravity (planets: PlanetScalar list) =
    planets |> List.map (applyGravityToPlanet planets)

let applyVelocity (planets: PlanetScalar list) =
    let singlePlanetVelocity (planet: PlanetScalar) =
        { planet with position = planet.position + planet.velocity }
    planets |> List.map singlePlanetVelocity

let runTimeStep (planets: PlanetScalar list): PlanetScalar list =
    planets
    |> applyGravity
    |> applyVelocity

let runTime (planets: PlanetScalar list): PlanetScalar list seq =
    planets |> Seq.unfold (fun planets -> Some(runTimeStep planets, runTimeStep planets))

let findCycle (initialPlanets: PlanetScalar list): int64 =
    let isInitialState (planets: PlanetScalar list): bool =
        List.compareWith Operators.compare planets initialPlanets = 0
    runTime initialPlanets
    |> Seq.findIndex isInitialState
    |> (fun i -> int64 i + 1L)

let initialPlanets =
    [ createPlanet (8, 0, 8)
      createPlanet (0, -5, -10)
      createPlanet (16, 10, -5)
      createPlanet (19, -10, -7) ]

let xPeriod = findCycle (initialPlanets |> List.map (toScalar (fun vector -> vector.x)))
let yPeriod = findCycle (initialPlanets |> List.map (toScalar (fun vector -> vector.y)))
let zPeriod = findCycle (initialPlanets |> List.map (toScalar (fun vector -> vector.z)))

let rec gcd (x: int64) (y: int64) =
    if y = 0L then abs x
    else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

printf "%A\r\n" (xPeriod, yPeriod, zPeriod)
printf "%d" (lcm xPeriod (lcm yPeriod zPeriod))
