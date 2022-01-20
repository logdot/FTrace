module SphereTests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit

open FTrace.Constants
open FTrace.Types.Tuples
open FTrace.Types.Shapes
open FTrace.Types.Matrix
open FTrace.Types.Material
open TestHelpers

[<Fact>]
let ``The normal on a sphere at a point on the x axis``() =
    let s = Sphere.Unit
    let n = s.normal (Point 1. 0. 0.)

    n |> should equal (Vector 1. 0. 0.)

[<Fact>]
let ``The normal on a sphere at a point on the y axis``() =
    let s = Sphere.Unit
    let n = s.normal (Point 0. 1. 0.)

    n |> should equal (Vector 0. 1. 0.)

[<Fact>]
let ``The normal on a sphere at a point on the z axis``() =
    let s = Sphere.Unit
    let n = s.normal (Point 0. 0. 1.)

    n |> should equal (Vector 0. 0. 1.)

[<Fact>]
let ``The normal on a sphere at a nonaxial point``() =
    let s = Sphere.Unit
    let n = s.normal (Point (sqrt 3./3.) (sqrt 3./3.) (sqrt 3./3.))

    n |> should equal (Vector (sqrt 3./3.) (sqrt 3./3.) (sqrt 3./3.))

[<Fact>]
let ``Computing the normal on a translated sphere``() =
    let s = Sphere.create (translate 0. 1. 0.) Material.Default
    let n = s.normal (Point 0. 1.70711 -0.70711)
    
    n .= (Vector 0. 0.70711 -0.70711) |> should equal true

[<Fact>]
let ``Computing the normal on a transformed sphere``() =
    let s = Sphere.create (scale 1. 0.5 1. * rotateZ (PI/5.)) Material.Default
    let n = s.normal (Point 0. (sqrt 2./2.) -(sqrt 2./2.))

    n .= (Vector 0. 0.97014 -0.24254) |> should equal true
