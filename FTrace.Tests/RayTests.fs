module RayTests

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit

open FTrace.Constants
open FTrace.Types.Ray
open FTrace.Types.Tuples
open FTrace.Types.Shapes
open FTrace.Types.Intersection
open FTrace.Types.Matrix
open FTrace.Types.Material
open TestHelpers

[<Fact>]
let ``Creating a ray``() =
    let origin = Point 1. 2. 3.
    let direction = Vector 4. 5. 6.
    let ray = Ray.create origin direction

    ray.Origin |> should equal origin
    ray.Direction |> should equal direction

[<Fact>]
let ``Computing a point from a distance``() =
    let origin = Point 2. 3. 4.
    let direction = Vector 1. 0. 0.

    let r = Ray.create origin direction

    r.position 0. |> should equal (Point 2. 3. 4.)
    r.position 1. |> should equal (Point 3. 3. 4.)
    r.position -1. |> should equal (Point 1. 3. 4.)
    r.position 2.5 |> should equal (Point 4.5 3. 4.)

[<Fact>]
let ``A ray intersects a sphere at two points``() =
    let origin = Point 0. 0. -5.
    let direction = Vector 0. 0. 1.

    let r = Ray.create origin direction
    let s = Sphere.Unit

    let intersects = s.intersect r

    intersects.Length |> should equal 2
    intersects.[0].T |> should equal 4.0
    intersects.[1].T |> should equal 6.0
    //intersects |> should equal [4.; 6.]

[<Fact>]
let ``A ray intersects a sphere at a tangent``() =
    let origin = Point 0. 1. -5.
    let direction = Vector 0. 0. 1.

    let r = Ray.create origin direction
    let s = Sphere.Unit

    let intersects = s.intersect r

    intersects.Length |> should equal 2
    intersects.[0].T |> should equal 5.0
    intersects.[1].T |> should equal 5.0
    //intersects |> should equal [5.; 5.]

[<Fact>]
let ``A ray misses a sphere``() =
    let origin = Point 0. 2. -5.
    let direction = Vector 0. 0. 1.

    let r = Ray.create origin direction
    let s = Sphere.Unit

    let intersects = s.intersect r

    intersects.Length |> should equal 0

[<Fact>]
let ``A ray originates inside of a sphere``() =
    let origin = Point 0. 0. 0.
    let direction = Vector 0. 0. 1.

    let r = Ray.create origin direction
    let s = Sphere.Unit

    let intersects = s.intersect r

    intersects.Length |> should equal 2
    intersects.[0].T |> should equal -1.0
    intersects.[1].T |> should equal 1.0
    //intersects |> should equal [-1.; 1.]

[<Fact>]
let ``A sphere is behind a ray``() =
    let origin = Point 0. 0. 5.
    let direction = Vector 0. 0. 1.

    let r = Ray.create origin direction
    let s = Sphere.Unit

    let intersects = s.intersect r

    intersects.Length |> should equal 2
    intersects.[0].T |> should equal -6.0
    intersects.[1].T |> should equal -4.0
    //intersects |> should equal [-6.; -4.]

[<Fact>]
let ``An intersect encapsulates t and object``() =
    let s = Sphere.Unit
    let i = Intersection.create s 3.5

    i.Object |> should equal s
    i.T |> should equal 3.5

[<Fact>]
let ``Aggregating intersections``() =
    let s = Sphere.Unit
    let i1 = Intersection.create s 1.
    let i2 = Intersection.create s 2.

    let xs = intersections [i1] i2

    xs.Length |> should equal 2
    xs.[0].T |> should equal 1.
    xs.[1].T |> should equal 2.

[<Fact>]
let ``The hit, when all intersections have positive t``() =
    let s = Sphere.Unit
    let i1 = Intersection.create s 1.
    let i2 = Intersection.create s 2.
    let xs = intersections [i2] i1

    let i = hit xs
    i.Value |> should equal i1

[<Fact>]
let ``The hit, when some intersections have negative t``() =
    let s = Sphere.Unit
    let i1 = Intersection.create s -1.
    let i2 = Intersection.create s 1.
    let xs = intersections [i1] i2

    let i = hit xs
    i.Value |> should equal i2

[<Fact>]
let ``The hit, when all intersections have negative t``() =
    let s = Sphere.Unit
    let i1 = Intersection.create s -2.
    let i2 = Intersection.create s -1.
    let xs = intersections [i1] i2

    let i = hit xs
    i.IsNone |> should equal true

[<Fact>]
let ``The hit is always the lowest nonnegative intersection``() =
    let s = Sphere.Unit
    let i1 = Intersection.create s 5.
    let i2 = Intersection.create s 7.
    let i3 = Intersection.create s -3.
    let i4 = Intersection.create s 2.
    let xs = [i1; i2; i3; i4]

    let i = hit xs
    i.Value |> should equal i4

[<Fact>]
let ``Translating a ray``() =
    let origin = Point 1. 2. 3.
    let direction = Vector 0. 1. 0.
    let r = Ray.create origin direction
    let m = translate 3. 4. 5.

    let r' = r.transform m
    r'.Origin |> should equal (Point 4. 6. 8.)
    r'.Direction |> should equal (Vector 0. 1. 0.)

[<Fact>]
let ``Scaling a ray``() =
    let origin = Point 1. 2. 3.
    let direction = Vector 0. 1. 0.
    let r = Ray.create origin direction
    let m = scale 2. 3. 4.

    let r' = r.transform m
    r'.Origin |> should equal (Point 2. 6. 12.)
    r'.Direction |> should equal (Vector 0. 3. 0.)

[<Fact>]
let ``A sphere has a default transform``() =
    let s = Sphere.Unit
    s.Transform |> should equal identity

[<Fact>]
let ``Intersecting a scaled sphere with a ray``() =
    let origin = Point 0. 0. -5.
    let direction = Vector 0. 0. 1.
    let r = Ray.create origin direction
    let s = Sphere.create (scale 2. 2. 2.) Material.Default
    
    let xs = s.intersect r
    xs.Length |> should equal 2
    xs.[0].T |> should equal 3.
    xs.[1].T |> should equal 7.

[<Fact>]
let ``Intersecting a translated sphere with a ray``() =
    let origin = Point 0. 0. -5.
    let direction = Vector 0. 0. 1.
    let r = Ray.create origin direction
    let s = Sphere.create (translate 5. 0. 0.) Material.Default
    
    let xs = s.intersect r
    xs.Length |> should equal 0