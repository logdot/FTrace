module Sphere

open System.IO
open FTrace.Types.Canvas
open FTrace.Types.Tuples
open FTrace.Types.Ray
open FTrace.Types.Shapes
open FTrace.Types.Intersection
open FTrace.Constants

let width = 500

let rayOrigin = Point 0. 0. -5.
let wallZ = 10.
let wallSize = 7.

let pixelSize = wallSize /. width
let half = wallSize / 2.

let canvas = Canvas.create width width
let color = Color 1. 0. 0.
let shape = Sphere.Unit

let run =
    for y in 0..width do
        let worldY = -half + pixelSize * (float <| y)

        for x in 0..width do
            let worldX = -half + pixelSize * (float <| x)

            let position = Point worldX worldY wallZ

            let ray = Ray.create rayOrigin (Tuple.normalize (position - rayOrigin))
            let xs = shape.intersect ray

            match hit(xs) with
            | Some i -> writePixel canvas x y color
            | None -> ()

    File.WriteAllText("Sphere.ppm", toPPM canvas)