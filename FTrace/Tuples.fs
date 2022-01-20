namespace FTrace.Types

open FTrace.Constants

type Tuplet =
    {
        X:float
        Y:float
        Z:float
        W:float
    }

    static member (.=)(a, b) =
        abs (a.X - b.X) < Epsilon
        && abs (a.Y - b.Y) < Epsilon
        && abs (a.Z - b.Z) < Epsilon
        && abs (a.W - b.W) < Epsilon 

    static member (+) (a:Tuplet, b:Tuplet) = Tuplet.create (a.X + b.X) (a.Y + b.Y) (a.Z + b.Z) (a.W + b.W)
    static member (-) (a:Tuplet, b:Tuplet) = Tuplet.create (a.X - b.X) (a.Y - b.Y) (a.Z - b.Z) (a.W - b.W)
    static member (~-) (a:Tuplet) = Tuplet.create (-a.X) (-a.Y) (-a.Z) (-a.W)
    static member (*) (a:Tuplet, b:float) = Tuplet.create (a.X * b) (a.Y * b) (a.Z * b) (a.W * b)
    static member (*) (a:Tuplet, b:Tuplet) = Tuplet.create (a.X * b.X) (a.Y * b.Y) (a.Z * b.Z) (a.W * b.W)
    static member (/) (a:Tuplet, b:float) = Tuplet.create (a.X / b) (a.Y / b) (a.Z / b) (a.W / b)

    static member (<*>) (a:Tuplet, b:Tuplet) = a.X*b.X + a.Y*b.Y + a.Z*b.Z + a.W*b.W
    static member (.*.) (a:Tuplet, b:Tuplet) = Tuplet.create (a.Y*b.Z - a.Z*b.Y) (a.Z*b.X - a.X*b.Z) (a.X*b.Y - a.Y*b.X) 0.0

    static member magnitude (a:Tuplet) = sqrt (a.X*a.X + a.Y*a.Y + a.Z*a.Z + a.W*a.W)
    static member normalize (a:Tuplet) = a / Tuplet.magnitude (a)

    static member create (x:float) (y:float) (z:float) (w:float) = { X=x; Y=y; Z=z; W=w }

module Tuples = 
    let Point x y z = Tuplet.create x y z 1.0
    let Vector x y z = Tuplet.create x y z 0.0
    let Color = Point 

    let reflect (inp:Tuplet) (normal:Tuplet) =
        inp - ((normal * 2.) * (inp <*> normal))

    module Colors =
      let black = Color 0.0 0.0 0.0
      let white = Color 1.0 1.0 1.0
      let red = Color 1.0 0.0 0.0
      let green = Color 0.0 1.0 0.0
      let blue = Color 0.0 0.0 1.0