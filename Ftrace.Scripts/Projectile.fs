open FTrace.Types.Tuples

type Projectile =
    { position: Tuple 
      velocity: Tuple }

type Environment =
    { gravity: Tuple
      wind: Tuple }

let tick env p =
    { position = p.position + p.velocity
      velocity = p.velocity + env.gravity + env.wind }

let mutable projectile = 
    { position = Point 0. 1. 0.
      velocity = Vector 1. 1. 0. |> Tuple.normalize }

let env =
    { gravity = Vector 0. -0.1 0.
      wind = Vector -0.01 0.0 0.0 }

let mutable i = 0

while projectile.position.Y > 0.0 do
    printf "\nTick: %-2d \nPosition: %A \nVelocity: %A\n" i projectile.position projectile.velocity
    projectile <- tick env projectile
    i <- i + 1