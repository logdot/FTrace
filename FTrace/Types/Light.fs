namespace FTrace.Types

open FTrace.Constants
open Tuples

type Light =
    { Position: Tuplet
      Intensity: Tuplet }

    static member create position intensity =
        { Position = position
          Intensity = intensity }

module Light =
    let lighting (material: Material) (light: Light) (point: Tuplet) (eyev: Tuplet) (normalv: Tuplet) =
        let effectiveColor = material.Color * light.Intensity
        let lightv = Tuplet.normalize (light.Position - point)
        let ambient = effectiveColor * material.Ambient

        let lightDotNormal = lightv <*> normalv

        if lightDotNormal < 0. then
            ambient
        else
            let diffuse = effectiveColor * material.Diffuse * lightDotNormal
            let reflectv = reflect -lightv normalv
            let reflectDotEye = reflectv <*> eyev

            if reflectDotEye < 0. then
                ambient + diffuse
            else
                let factor = pown reflectDotEye (int <| material.Shininess)
                let specular = light.Intensity * material.Specular * factor

                ambient + diffuse + specular
