namespace FTrace.Types

open FTrace.Constants

type 'T Intersection =
    {
        Object:'T
        T:float
    }

    static member create obj (t:float) = {
        Object=obj;
        T=t
    }

module Intersection =
    let hit (input:'T Intersection list) =
        input
        |> List.sortBy (fun i -> i.T)
        |> List.where (fun i -> i.T >= 0.0)
        |> List.tryHead