namespace FTrace.Types.Shapes

open FTrace.Types
open FTrace.Types.Tuples

type Sphere =
    { Transform: Matrix
      Material: Material }

    member x.intersect = (x :> IIntersectable).intersect

    interface IIntersectable with
        member x.intersect(ray: Ray) =
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

                let x' = x :> IIntersectable

                let i1 = Intersection<IIntersectable>.create x' t1
                let i2 = Intersection<IIntersectable>.create x' t2

                [ i1; i2 ]

    member x.transform(m: Matrix) = { x with Transform = m }

    member x.normal(worldP: Tuplet) =
        let objectP = x.Transform.inverse * worldP
        let objectN = objectP - Point 0. 0. 0.
        let worldN = x.Transform.inverse.transpose * objectN
        Tuplet.normalize { worldN with W = 0. }

    static member create transform material =
        { Transform = transform
          Material = material }

module Sphere =
    let Unit = Sphere.create identity Material.Default
