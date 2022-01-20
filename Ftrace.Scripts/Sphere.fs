module Sphere

open System.IO
open FTrace.Types
open FTrace.Types.Canvas
open FTrace.Types.Tuples
open FTrace.Types.Shapes
open FTrace.Types.Intersection
open FTrace.Types.Light
open FTrace.Types.Matrix
open FTrace.Constants

let width = 500

let rayOrigin = Point 0. 0. -5.
let wallZ = 10.
let wallSize = 7.

let pixelSize = wallSize /. width
let half = wallSize / 2.

let canvas = Canvas.create width width
//let shape = Sphere.Unit
let shape = Sphere.create (scale 1. 0.5 1.) Material.Default

let lightPos = Point -10. 10. -10.
let lightColor = Color 1. 1. 1.
let light = Light.create lightPos lightColor

let linearInterpolation x1 x2 y1 y2 x :float =
    y1 + (x - x1) * ((y2 - y1) / (x2 - x1))

let run =
    for y in 0..width do
        let worldY = -half + pixelSize * (float <| y)

        for x in 0..width do
            let worldX = -half + pixelSize * (float <| x)

            let position = Point worldX -worldY wallZ

            let ray = Ray.create rayOrigin (Tuplet.normalize (position - rayOrigin))
            let xs = shape.intersect ray

            match hit(xs) with
            | Some i ->
                let point = ray.position i.T
                let normal = i.Object.normal point
                let eye = -ray.Direction

                let color = lighting i.Object.Material light point eye normal
                writePixel canvas x y color
            | None -> ()

    File.WriteAllText("Sphere.ppm", toPPM canvas)