open System.IO
open FTrace.Types.Tuples
open FTrace.Types.Canvas

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
      velocity = (Vector 1. 1.8 0. |> Tuple.normalize) * 5. }

let env =
    { gravity = Vector 0. -0.1 0.
      wind = Vector -0.01 0.0 0.0 }

let canvas = Canvas.create 200 100 
let color = Color 1. 0. 0.

let mutable i = 0

while projectile.position.Y > 0.0 do
    let canvasX =
        projectile.position.X
        |> round
        |> int

    let canvasY = canvas.Height - (round projectile.position.Y |> int)

    if canvasX >= 0 && canvasX < canvas.Width && canvasY >= 0 && canvasY < canvas.Height then
        writePixel canvas canvasX canvasY color

    printf "\nTick: %-2d \nPosition: %A \nVelocity: %A\n" i projectile.position projectile.velocity
    projectile <- tick env projectile
    i <- i + 1

File.WriteAllText("Projectile.ppm", toPPM canvas)