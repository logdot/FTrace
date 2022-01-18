module Clock

open System.IO
open FTrace.Types.Canvas
open FTrace.Types.Tuples
open FTrace.Types.Matrix
open FTrace.Constants

let width = 500
let height = width

let angle = 360*4

let canvas = Canvas.create width height 
let color = Color 1. 0. 0.

let mutable p = Point 0. (width ./ 4.) 0.

let clockRot = rotateZ (PI / (angle ./ 2.))

let run =
    for i in 1..angle do
        let x = int <| width ./ 2. - p.X
        let y = int <| height ./ 2. - p.Y
        writePixel canvas x y color

        p <- clockRot * p

    File.WriteAllText("Clock.ppm", toPPM canvas)