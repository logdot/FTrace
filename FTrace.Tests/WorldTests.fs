module WorldTests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit

open FTrace.Types
open FTrace.Types.Shapes
open FTrace.Types.Tuples

[<Fact>]
let ``Creating a world`` () =
    let w = World.create

    w.Light |> should equal None
    w.Objects |> should be Empty

[<Fact>]
let ``Have a default world`` () =
    let light = Light.create (Point -10. 10. -10.) (Point 1. 1. 1.)

    let s1 =
        Sphere.create identity (Material.create (Point 0.8 1. 0.6) 0.1 0.7 0.2 200.) :> IIntersectable

    let s2 = Sphere.create (scale 0.5 0.5 0.5) Material.Default :> IIntersectable

    let w = World.Default

    w.Light |> should equal (Some light)
    List.contains s1 w.Objects |> should be True
    List.contains s2 w.Objects |> should be True

[<Fact>]
let ``Intersect a world with a ray`` () =
    let w = World.Default
    let r = Ray.create (Point 0. 0. -5.) (Vector 0. 0. 1.)

    let xs = w.intersect r

    xs.Length |> should equal 4
    xs[0].T |> should equal 4.
    xs[1].T |> should equal 4.5
    xs[2].T |> should equal 5.5
    xs[3].T |> should equal 6.
