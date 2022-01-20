namespace FTrace.Types

open FTrace.Constants

type Ray = 
    {
        Origin:Tuplet
        Direction:Tuplet 
    }

    member x.position (t:float) = x.Origin + x.Direction * t
    member x.transform (m:Matrix) =
        let origin = m * x.Origin
        let direction = m * x.Direction
        Ray.create origin direction

    static member create origin direction = { Origin=origin; Direction=direction }