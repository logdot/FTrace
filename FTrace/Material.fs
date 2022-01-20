namespace FTrace.Types

open Tuples

module Material =
    type Material =
        {
            Color:Tuple
            Ambient:float
            Diffuse:float
            Specular:float
            Shininess:float
        }

        static member create color ambient diffuse specular shininess = {
            Color=color
            Ambient=ambient
            Diffuse=diffuse
            Specular=specular
            Shininess=shininess }

        static member Default =
            let color = Point 1. 0. 0.
            Material.create color 0.1 0.9 0.9 200.