module MatrixTests

open Xunit
open FsUnit.Xunit
open FsCheck
//open FsCheck.FSharp
open FsCheck.Xunit

open FTrace.Constants
open FTrace.Types.Matrix
open FTrace.Types.Tuples
open TestHelpers

module Creation =
    [<Fact>]
    let ``Creating a matrix``() =
        let matrix = Matrix.create [
            [1.; 2.; 3.; 4.]
            [5.5; 6.5; 7.5; 8.5]
            [9.; 10.; 11.; 12.]
            [13.5; 14.5; 15.5; 16.5] ]

        matrix.[0, 0] |> should equal 1.
        matrix.[0, 3] |> should equal 4.
        matrix.[1, 0] |> should equal 5.5
        matrix.[1, 2] |> should equal 7.5
        matrix.[2, 2] |> should equal 11.
        matrix.[3, 0] |> should equal 13.5
        matrix.[3, 2] |> should equal 15.5

    [<Fact>]
    let ``Creating a 2x2 matrix``() =
        let matrix = Matrix.create [
            [1.; 2.]
            [3.; 4.] ]

        matrix.[0, 0] |> should equal 1.
        matrix.[0, 1] |> should equal 2.
        matrix.[1, 0] |> should equal 3.
        matrix.[1, 1] |> should equal 4.

    [<Fact>]
    let ``Creating a 3x3 matrix``() =
        let matrix = Matrix.create [
            [1.; 2.; 3.]
            [4.; 5.; 6.]
            [7.; 8.; 9.] ]

        matrix.[0, 0] |> should equal 1.
        matrix.[0, 1] |> should equal 2.
        matrix.[0, 2] |> should equal 3.
        matrix.[1, 0] |> should equal 4.
        matrix.[1, 1] |> should equal 5.
        matrix.[1, 2] |> should equal 6.
        matrix.[2, 0] |> should equal 7.
        matrix.[2, 1] |> should equal 8.
        matrix.[2, 2] |> should equal 9.

module Equality =
    [<Fact>]
    let ``Matrix equality with identical matrices``() =
        let matrix1 = Matrix.create [
            [1.; 2.; 3.]
            [4.; 5.; 6.]
            [7.; 8.; 9.] ]

        let matrix2 = Matrix.create [
            [1.; 2.; 3.]
            [4.; 5.; 6.]
            [7.; 8.; 9.] ]

        matrix1 .= matrix2 |> should equal true

    [<Fact>]
    let ``Matrix equality with non identical matrices``() =
        let matrix1 = Matrix.create [
            [1.; 2.; 3.]
            [4.; 5.; 6.]
            [7.; 8.; 9.] ]
        let matrix2 = Matrix.create [
            [1.; 2.; 3.]
            [4.; 5.; 6.]
            [7.; 8.; 10.] ]

        matrix1 .= matrix2 |> should equal false

module Multiplication =
    [<Fact>]
    let ``Multiplying two matrices``() =
        let matrix1 = Matrix.create [
            [1.; 2.; 3.; 4.;]
            [5.; 6.; 7.; 8.]
            [9.; 8.; 7.; 6.]
            [5.; 4.; 3.; 2.] ]

        let matrix2 = Matrix.create [
            [-2.; 1.; 2.; 3.]
            [3.; 2.; 1.; -1.]
            [4.; 3.; 6.; 5.]
            [1.; 2.; 7.; 8.] ]

        let result = Matrix.create [
            [20.; 22.; 50.; 48.]
            [44.; 54.; 114.; 108.]
            [40.; 58.; 110.; 102.]
            [16.; 26.; 46.; 42.] ]

        matrix1 * matrix2 |> should equal result

    [<Fact>]
    let ``Multiplying a matrix with a tuple``() =
        let matrix = Matrix.create [
            [1.; 2.; 3.; 4.]
            [2.; 4.; 4.; 2.]
            [8.; 6.; 4.; 1.]
            [0.; 0.; 0.; 1.] ]

        let tuple = Tuple.create 1. 2. 3. 1.

        let result = Tuple.create 18. 24. 33. 1.

        matrix * tuple .= result |> should equal true

    [<Fact>]
    let ``Multiplying a matrix with the identity matrix``() =
        let matrix = Matrix.create [
            [0.; 1.; 2.; 4.]
            [1.; 2.; 4.; 8.]
            [2.; 4.; 8.; 16.]
            [4.; 8.; 16.; 32.] ]

        matrix * identity |> should equal matrix

module Transposing =
    [<Fact>]
    let ``Transposing a matrix``() =
        let matrix = Matrix.create [
            [0.; 9.; 3.; 0.]
            [9.; 8.; 0.; 8.]
            [1.; 8.; 5.; 3.]
            [0.; 0.; 5.; 8.] ]

        let result = Matrix.create [
            [0.; 9.; 1.; 0.]
            [9.; 8.; 8.; 0.]
            [3.; 0.; 5.; 5.]
            [0.; 8.; 3.; 8.] ]

        matrix.transpose |> should equal result

    [<Fact>]
    let ``Transpose identity matrix``() =
        identity.transpose |> should equal identity
    
[<Fact>]
let ``Determinant of 2x2 matrix``() =
    let matrix = Matrix.create [
        [1.; 5.]
        [-3.; 2.] ]

    matrix.determinant |> should equal 17.

module Submatrixes =
    [<Fact>]
    let ``A submatrix of a 3x3 matrix is a 2x2 matrix``() =
        let matrix = Matrix.create [
            [1.; 5.; 0.]
            [-3.; 2.; 7.]
            [0.; 6.; -3.] ]

        let result = Matrix.create [
            [-3.; 2.]
            [0.; 6.] ]

        matrix.submatrix 0 2 |> should equal result

    [<Fact>]
    let ``A submatrix of a 4x4 matrix is a 3x3 matrix``() =
        let matrix = Matrix.create [
            [-6.; 1.; 1.; 6.]
            [-8.; 5.; 8.; 6.]
            [-1.; 0.; 8.; 2.]
            [-7.; 1.; -1.; 1.] ]

        let result = Matrix.create [
            [-6.; 1.; 6.]
            [-8.; 8.; 6.]
            [-7.; -1.; 1.] ]

        matrix.submatrix 2 1 |> should equal result

[<Fact>]
let ``Calculating a minor of a 3x3 matrix``() =
    let matrix = Matrix.create [
        [3.; 5.; 0.]
        [2.; -1.; -7.]
        [6.; -1.; 5.]]

    let b = matrix.submatrix 1 0

    b.determinant |> should equal (matrix.minor 1 0)

[<Fact>]
let ``Calculating a cofactor of a 3x3 matrix``() =
    let a = Matrix.create [
        [3.; 5.; 0.]
        [2.; -1.; -7.]
        [6.; -1.; 5.]]
    
    a.minor 0 0 |> should equal (a.cofactor 0 0)
    a.minor 1 0 |> should not' (equal (a.cofactor 1 0))

    a.minor 0 0 .=. -12. |> should equal true
    a.cofactor 0 0 .=. -12. |> should equal true
    a.minor 1 0 .=. 25. |> should equal true
    a.cofactor 1 0 .=. -25. |> should equal true

[<Fact>]
let ``Calculating the determinant of a 3x3 matrix``() =
    let a = Matrix.create [
        [1.; 2.; 6.]
        [-5.; 8.; -4.]
        [2.; 6.; 4.]]

    a.cofactor 0 0 .=. 56. |> should equal true
    a.cofactor 0 1 .=. 12. |> should equal true
    a.cofactor 0 2 .=. -46. |> should equal true
    a.determinant .=. -196. |> should equal true

[<Fact>]
let ``Calculating the determinant of a 4x4 matrix``() =
    let a = Matrix.create [
        [-2.; -8.; 3.; 5.]
        [-3.; 1.; 7.; 3.]
        [1.; 2.; -9.; 6.]
        [-6.; 7.; 7.; -9.]]
    
    a.cofactor 0 0 .=. 690. |> should equal true
    a.cofactor 0 1 .=. 447. |> should equal true
    a.cofactor 0 2 .=. 210. |> should equal true
    a.cofactor 0 3 .=. 51. |> should equal true
    a.determinant .=. -4071. |> should equal true

module Invertibility =
    [<Fact>]
    let ``Testing the invertibility of an invertible matrix``() =
        let a = Matrix.create [
            [6.; 4.; 4.; 4.]
            [5.; 5.; 7.; 6.]
            [4.; -9.; 3.; -7.]
            [9.; 1.; 7.; -6.] ]

        a.invertible |> should equal true

    [<Fact>]
    let ``Testing the invertibility of an uninvertible matrix``() =
        let a = Matrix.create [
            [-4.; 2.; -2.; -3.]
            [9.; 6.; 2.; 6.]
            [0.; -5.; 1.; -5.]
            [0.; 0.; 0.; 0.] ]

        a.invertible |> should equal false

    [<Fact>]
    let ``Calculating the inverse of a matrix``() =
        let a = Matrix.create [
            [-5.; 2.; 6.; -8.;]
            [1.; -5.; 1.; 8.]
            [7.; 7.; -6.; -7.]
            [1.; -3.; 7.; 4.] ]
        let b:Matrix = a.inverse
        
        let result = Matrix.create [
            [0.21805; 0.45113; 0.24060; -0.04511]
            [-0.80827; -1.45677; -0.44361; 0.52068]
            [-0.07895; -0.22368; -0.05263; 0.19737]
            [-0.52256; -0.81391; -0.30075; 0.30639] ]

        a.determinant .=. 532. |> should equal true
        a.cofactor 2 3 .=. -160. |> should equal true
        b.[3, 2] .=. (-160./532.) |> should equal true
        a.cofactor 3 2 .=. 105. |> should equal true
        b.[2, 3] .=. (105./532.) |> should equal true

        b .= result |> should equal true

    [<Fact>]
    let ``Calculating the inverse of another matrix``() =
        let a = Matrix.create [
            [8.; -5.; 9.; 2.]
            [7.; 5.; 6.; 1.]
            [-6.; 0.; 9.; 6.]
            [-3.; 0.; -9.; -4.] ]

        let result = Matrix.create [
            [-0.15385; -0.15385; -0.28205; -0.53846]
            [-0.07692; 0.12308; 0.02564; 0.03077]
            [0.35897; 0.35897; 0.43590; 0.92308]
            [-0.69231; -0.69231; -0.76923; -1.92308] ]
        
        a.inverse .= result |> should equal true

    [<Fact>]
    let ``Calculating the inverse of a third matrix``() =
        let a = Matrix.create [
            [9.; 3.; 0.; 9.]
            [-5.; -2.; -6.; -3.]
            [-4.; 9.; 6.; 4.]
            [-7.; 6.; 6.; 2.] ]

        let result = Matrix.create [
            [-0.04074; -0.07778; 0.14444; -0.22222]
            [-0.07778; 0.03333; 0.36667; -0.33333]
            [-0.02901; -0.14630; -0.10926; 0.12963]
            [0.17778; 0.06667; -0.26667; 0.33333] ]

        a.inverse .= result |> should equal true

module Rotation =
    [<Fact>]
    let ``Rotating a point a quarter circle on the Y axis`` =
        let p = Point 0. 1. 0.
        let quarter = rotateY (PI/2.)

        quarter * p = Point 0. 0. 1. |> should equal true

type MatrixGenerator =
    static member matrix() =
        let size = Gen.choose(2, 4)

        Gen.elements [-100.0..0.01..100.0]
        |> Gen.listOfLength 4
        |> Gen.listOfLength 4
        |> Arb.fromGen

[<Property(Arbitrary=[|typeof<MatrixGenerator>|])>]
let ``PROPERTY: Multiplying a matrix by it's inverse is identity``(input:list<list<float>>) =
    let a = Matrix.create input

    a.invertible ==> (lazy(identity .= a * a.inverse))

[<Property(Arbitrary=[|typeof<MatrixGenerator>|])>]
let ``PROPERTY: Inverse of the transpose is equal to the transpose of the inverse``(input:list<list<float>>) =
    let a = Matrix.create input

    a.invertible ==> lazy(a.inverse.transpose .= a.transpose.inverse)

[<Property(Arbitrary=[|typeof<MatrixGenerator>|])>]
let ``PROPERTY: Multiplying a product by the inverse of one of the elements gives the other element``(a:list<list<float>>, b:list<list<float>>) =
    let a = Matrix.create a
    let b = Matrix.create b

    let c = a * b

    b.invertible ==> lazy(c * b.inverse .= a)