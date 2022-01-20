module LightTests

open System
open Xunit
open FsUnit.Xunit
open FTrace.Types
open FTrace.Types.Tuples
open FTrace.Types.Light

[<Fact>]
let ``A point light has a position and a intensity``() =
    let position = Point 0. 0. 0.
    let intensity = Color 1. 1. 1.
    let light = Light.create position intensity

    light.Position |> should equal position
    light.Intensity |> should equal intensity

let whiteMaterial = {Material.Default with Color=Color 1. 1. 1.}
[<Fact>]
let ``Lighting with the eye between the light and the surface``() =
    let m = whiteMaterial
    let position = Point 0. 0. 0.

    let eyev = Vector 0. 0. -1.
    let normalv = Vector 0. 0. -1.
    let light = Light.create (Point 0. 0. -10.) (Color 1. 1. 1.)
    let result = lighting m light position eyev normalv
    result |> should equal (Tuplet.create 1.9 1.9 1.9 1.9)

[<Fact>]
let ``Lighting with the eye between the light and the surface 45 degree offset``() =
    let m = whiteMaterial
    let position = Point 0. 0. 0.

    let eyev = Vector 0. (sqrt 2./2.) -(sqrt 2./2.)
    let normalv = Vector 0. 0. -1.
    let light = Light.create (Point 0. 0. -10.) (Color 1. 1. 1.)
    let result = lighting m light position eyev normalv
    result |> should equal (Color 1. 1. 1.)

[<Fact>]
let ``Lighting with the eye opposite surface light offset 45 degrees``() =
    let m = whiteMaterial
    let position = Point 0. 0. 0.

    let eyev = Vector 0. 0. -1.
    let normalv = Vector 0. 0. -1.
    let light = Light.create (Point 0. 10. -10.) (Color 1. 1. 1.)
    let result = lighting m light position eyev normalv
    result .= (Tuplet.create 0.7364 0.7364 0.7364 0.7364) |> should equal true

[<Fact>]
let ``Lighting with the eye in the path of the reflection vector``() =
    let m = whiteMaterial
    let position = Point 0. 0. 0.

    let eyev = Vector 0. -(sqrt 2./2.) -(sqrt 2./2.)
    let normalv = Vector 0. 0. -1.
    let light = Light.create (Point 0. 10. -10.) (Color 1. 1. 1.)
    let result = lighting m light position eyev normalv
    result .= (Tuplet.create 1.63639 1.63639 1.63639 1.63639) |> should equal true

[<Fact>]
let ``Lighting with the light behind the surface``() =
    let m = whiteMaterial
    let position = Point 0. 0. 0.

    let eyev = Vector 0. 0. -1.
    let normalv = Vector 0. 0. -1.
    let light = Light.create (Point 0. 0. 10.) (Color 1. 1. 1.)
    let result = lighting m light position eyev normalv
    result .= (Tuplet.create 0.1 0.1 0.1 0.1) |> should equal true