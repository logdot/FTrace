namespace FTrace.Types.Shapes

open FTrace.Types.Tuples
open FTrace.Types.Intersection
open FTrace.Types.Ray
open FTrace.Types.Matrix
open FTrace.Types.Material

module Sphere =
    type Sphere =
        { 
            Transform:Matrix
            Material:Material
        }

        member x.intersect (ray:Ray) =
            let iRay = ray.transform x.Transform.inverse

            let str = iRay.Origin - Point 0. 0. 0.

            let a = iRay.Direction <*> iRay.Direction
            let b = 2.0 * (iRay.Direction <*> str)
            let c = (str <*> str) - 1.
            let discriminant = b * b - 4.0 * a * c

            if discriminant < 0.0 then
                []
            else
                let t1 = (-b - sqrt (discriminant)) / (2.0 * a)
                let t2 = (-b + sqrt (discriminant)) / (2.0 * a)

                let i1 = Intersection<Sphere>.create x t1
                let i2 = Intersection<Sphere>.create x t2

                [i1; i2]
            
        member x.transform (m:Matrix) = { x with Transform=m}
        member x.normal (worldP:Tuple) =
            let objectP = x.Transform.inverse * worldP
            let objectN = objectP - Point 0. 0. 0.
            let worldN = x.Transform.inverse.transpose * objectN
            Tuple.normalize { worldN with W=0. }
            

    let create transform material =
        {
            Transform=transform
            Material=material
        }

    let Unit =
        create identity Material.Default