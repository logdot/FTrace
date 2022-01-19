namespace FTrace.Types

open FTrace.Constants
open Tuples
open Matrix

module Ray =
    type Ray = 
        {
            Origin:Tuple
            Direction:Tuple 
        }

        static member create origin direction = { Origin=origin; Direction=direction }

        member x.position (t:float) = x.Origin + x.Direction * t
        member x.transform (m:Matrix) =
            let origin = m * x.Origin
            let direction = m * x.Direction
            Ray.create origin direction