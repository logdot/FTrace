module TupleTests

open System
open Xunit
open FsUnit.Xunit
open FTrace.Types
open FTrace.Types.Tuples

open TestHelpers

module Arithmetic = 
    [<Fact>]
    let ``Adding two tuples`` () =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = Tuplet.create 5. 6. 7. 8.
        let c = a + b
        c.X |> should equal 6.
        c.Y |> should equal 8.
        c.Z |> should equal 10.
        c.W |> should equal 12.

    [<Fact>]
    let ``Substracting two tuples``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = Tuplet.create 5. 6. 7. 8.
        let c = a - b
        c.X |> should equal -4.
        c.Y |> should equal -4.
        c.Z |> should equal -4.
        c.W |> should equal -4.

    [<Fact>]
    let ``Negating a tuple``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = -a
        b.X |> should equal -1.
        b.Y |> should equal -2.
        b.Z |> should equal -3.
        b.W |> should equal -4.

    [<Fact>]
    let ``Multiplying a tuple by a scalar``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = a * 3.
        b.X |> should equal 3.
        b.Y |> should equal 6.
        b.Z |> should equal 9.
        b.W |> should equal 12.

    [<Fact>]
    let ``Multiplying a tuple by a fraction``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = a * 0.5
        b.X |> should equal 0.5
        b.Y |> should equal 1.0
        b.Z |> should equal 1.5
        b.W |> should equal 2.0

    [<Fact>]
    let ``Dividing a tuple by a scalar``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = a / 2.
        b.X |> should equal 0.5
        b.Y |> should equal 1.0
        b.Z |> should equal 1.5
        b.W |> should equal 2.0

    [<Fact>]
    let ``Multiplication and division are the same``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = a * 0.5
        let c = a / 2.0
        b |> should equal b
        c |> should equal b

module Magnitude =
    [<Fact>]
    let ``Magnitude of a 1 0 0 0 tuple``() =
        let a = Tuplet.create 1. 0. 0. 0.
        let b = Tuplet.magnitude a
        b |> should equal 1.

    [<Fact>]
    let ``Magnitude of a 0 1 0 0 tuple``() =
        let a = Tuplet.create 0. 1. 0. 0.
        let b = Tuplet.magnitude a
        b |> should equal 1.

    [<Fact>]
    let ``Magnitude of a 0 0 1 0 tuple``() =
        let a = Tuplet.create 0. 0. 1. 0.
        let b = Tuplet.magnitude a
        b |> should equal 1.

    [<Fact>]
    let ``Magnitude of a 0 0 0 1 tuple``() =
        let a = Tuplet.create 0. 0. 0. 1.
        let b = Tuplet.magnitude a
        b |> should equal 1.

    [<Fact>]
    let ``Magnitude of a 1 2 3 4 tuple``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = Tuplet.magnitude a
        b .=. 5.477225575 |> should equal true

    [<Fact>]
    let ``Magnitude of a -1 -2 -3 -4 tuple``() =
        let a = Tuplet.create -1. -2. -3. -4.
        let b = Tuplet.magnitude a
        b .=. 5.477225575 |> should equal true

module Normalize =
    [<Fact>]
    let ``Normalized 4 0 0 0 vector is 1 0 0 0``() =
        let a = Tuplet.create 4. 0. 0. 0.
        let b = Tuplet.normalize a
        b.X |> should equal 1.
        b.Y |> should equal 0.
        b.Z |> should equal 0.
        b.W |> should equal 0.

    [<Fact>]
    let ``Magnitude of a normalized tuple is 1``() =
        let a = Tuplet.create 1. 3. 4. -2.
        let b = Tuplet.normalize a
        let c = Tuplet.magnitude b
        c .=. 1.0 |> should equal true


module Dot =
    [<Fact>]
    let ``Dot product of two tuples``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = Tuplet.create 2. 3. 4. 5.
        let c = a <*> b
        c |> should equal 40.

    [<Fact>]
    let ``Cross product of two tuples``() =
        let a = Tuplet.create 1. 2. 3. 4.
        let b = Tuplet.create 2. 3. 4. 5.
        let ab = a .*. b
        let ba = b .*. a
        ab |> should equal (Tuplet.create -1. 2. -1. 0.)
        ba |> should equal (Tuplet.create 1. -2. 1. 0.)

module Reflection =
    [<Fact>]
    let ``Reflecting a vector approaching at 45deg``() =
        let v = Vector 1. -1. 0.
        let n = Vector 0. 1. 0.
        let r = reflect v n

        r |> should equal (Vector 1. 1. 0.)

    [<Fact>]
    let ``Reflecting a vector of a slanted surface``() =
        let v = Vector 0. -1. 0.
        let n = Vector (sqrt 2./2.) (sqrt 2./2.) 0.
        let r = reflect v n

        r .= (Vector 1. 0. 0.) |> should equal true
