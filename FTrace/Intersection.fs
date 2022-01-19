namespace FTrace.Types

open FTrace.Constants

module Intersection =
    type 'T Intersection =
        {
            Object:'T
            T:float
        }

        static member create obj (t:float) = {
            Object=obj;
            T=t
        }
        
    let intersections (a:'T Intersection list) (b:'T Intersection) = a@[b]

    let hit (input:'T Intersection list) =
        input
        |> List.sortBy (fun i -> i.T)
        |> List.where (fun i -> i.T >= 0.0)
        |> List.tryHead