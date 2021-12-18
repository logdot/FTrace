namespace FTrace.Types

module Tuples = 
    type Tuple =
        { X:float
          Y:float
          Z:float
          W:float }

        static member (+) (a:Tuple, b:Tuple) = Tuple.create (a.X + b.X) (a.Y + b.Y) (a.Z + b.Z) (a.W + b.W)
        static member (-) (a:Tuple, b:Tuple) = Tuple.create (a.X - b.X) (a.Y - b.Y) (a.Z - b.Z) (a.W - b.W)
        static member (~-) (a:Tuple) = Tuple.create (-a.X) (-a.Y) (-a.Z) (-a.W)
        static member (*) (a:Tuple, b:float) = Tuple.create (a.X * b) (a.Y * b) (a.Z * b) (a.W * b)
        static member (/) (a:Tuple, b:float) = Tuple.create (a.X / b) (a.Y / b) (a.Z / b) (a.W / b)

        static member (<*>) (a:Tuple, b:Tuple) = a.X*b.X + a.Y*b.Y + a.Z*b.Z + a.W*b.W
        static member (.*.) (a:Tuple, b:Tuple) = Tuple.create (a.Y*b.Z - a.Z*b.Y) (a.Z*b.X - a.X*b.Z) (a.X*b.Y - a.Y*b.X) 0.0

        static member magnitude (a:Tuple) = sqrt (a.X*a.X + a.Y*a.Y + a.Z*a.Z + a.W*a.W)
        static member normalize (a:Tuple) = a / Tuple.magnitude (a)
    
        static member create (x:float) (y:float) (z:float) (w:float) = { X=x; Y=y; Z=z; W=w }

    let Point x y z = Tuple.create x y z 1.0
    
    let Vector x y z = Tuple.create x y z 0.0

    let Color = Point 

    module Colors =
      let black = Color 0.0 0.0 0.0
      let white = Color 1.0 1.0 1.0
      let red = Color 1.0 0.0 0.0
      let green = Color 0.0 1.0 0.0
      let blue = Color 0.0 0.0 1.0