namespace FTrace.Types

open Tuples
open FTrace.Types.Shapes

type World =
    { Light: Option<FTrace.Types.Light>
      Objects: list<IIntersectable> }

    member x.intersect = (x :> IIntersectable).intersect

    interface IIntersectable with
        member x.intersect(ray: Ray) =
            x.Objects
            |> List.map (fun i -> i.intersect ray)
            |> List.concat
            |> List.sortBy (fun i -> i.T)

    static member Default =
        { Light = Some(Light.create (Point -10. 10. -10.) (Point 1. 1. 1.))
          Objects =
            [ Sphere.create identity (Material.create (Point 0.8 1. 0.6) 0.1 0.7 0.2 200.) :> IIntersectable
              Sphere.create (scale 0.5 0.5 0.5) Material.Default :> IIntersectable ] }

    static member create = { Light = None; Objects = [] }
