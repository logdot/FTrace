namespace FTrace.Types

open System
open FTrace.Constants
open Tuples

module Matrix =
    type Matrix =
        { Dimension:int
          Elements:float[,] }

        static member create rows = 
            let dim = List.length rows

            if dim >= 2
               && dim <= 4
               && List.forall (fun l -> List.length l = dim) rows then
                { Dimension=dim
                  Elements=array2D rows }
            else
                invalidArg "rows" "Invalid matrix dimensions"

        static member (.=)(m1, m2:Matrix) =
            let allWithingEpsilon =
                let len = m1.Dimension - 1

                seq {
                    for r in 0..len do
                        for c in 0..len do
                            let e1 = m1.[r, c]
                            let e2 = m2.[r, c]

                            yield abs (e1 - e2) < Epsilon
                }
                |> Seq.forall id

            m1.Dimension = m2.Dimension && allWithingEpsilon 

        static member (*)(m1, m2) =
            let len = m1.Dimension - 1

            let result =
                Array2D.zeroCreate m1.Dimension m1.Dimension

            for r in 0 .. len do
                for c in 0 .. len do
                    let row = m1.Elements.[r, *]
                    let col = m2.Elements.[*, c]

                    Array.fold2 (fun sum r c -> sum + r * c) 0.0 row col
                    |> Array2D.set result r c

            { Dimension=m1.Dimension
              Elements=result }

        static member (*)(m1, t:Tuple) =
            let len = m1.Dimension - 1

            let tArr = [| t.X; t.Y; t.Z; t.W|]

            let result = 
                [| for r in 0..len -> 
                    let row = m1.Elements.[r, *]
                    Array.fold2 (fun sum r c -> sum + r * c) 0.0 row tArr |]

            Tuple.create (result.[0]) (result.[1]) (result.[2]) (result.[3])

        member x.Item 
            with get (r, c) = x.Elements.[r, c]

        member x.transpose =
            [ for c in [0..x.Dimension - 1] do
                yield x.Elements.[*, c] |> List.ofArray ]
            |> Matrix.create

        member x.determinant =
            if x.Dimension = 2 then
                x.Elements.[0, 0] * x.Elements.[1, 1] - x.Elements.[0, 1] * x.Elements.[1, 0]
            elif x.Dimension = 0 then
                0.
            else
                Array.mapi (fun i entry -> entry * x.cofactor 0 i) x.Elements.[0, *]
                |> Array.sum

        member x.invertible =
            abs x.determinant > Epsilon

        member x.inverse =
            if x.invertible = false then
                failwith "Matrix is not invertible"

            let dim = x.Dimension
            let elements = Array2D.zeroCreate dim dim

            for r = 0 to dim-1 do
                for c = 0 to dim-1 do
                    let cofactor = x.cofactor r c

                    Array2D.set elements c r (cofactor / x.determinant)

            { Dimension=dim
              Elements=elements }

        member x.submatrix row col =
            let dim = x.Dimension - 1
            let elements = Array2D.zeroCreate dim dim

            for r = 0 to dim do
                if r <> row then
                    let r' = if r < row then r else r - 1

                    for c = 0 to dim do
                        if c <> col then
                            let c' = if c < col then c else c - 1

                            Array2D.set elements r' c' x.Elements.[r, c]
                            
            { Dimension=dim
              Elements=elements }
            
        member x.minor row col =
            let x' = x.submatrix row col
            x'.determinant

        member x.cofactor row col =
            let x' = x.minor row col
            if (row + col) % 2 = 1 then
                -x'
            else
                x'

    let identity = Matrix.create [
        [1.; 0.; 0.; 0.]
        [0.; 1.; 0.; 0.]
        [0.; 0.; 1.; 0.]
        [0.; 0.; 0.; 1.] ]