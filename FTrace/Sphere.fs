namespace FTrace.Types.Shapes

open FTrace.Types.Tuples
open FTrace.Types.Intersection
open FTrace.Types.Ray
open FTrace.Types.Matrix

module Sphere =
    type Sphere =
        { 
            Transform:Matrix
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

    let create transform =
        {
            Transform=transform
        }

    let Unit =
        create identity