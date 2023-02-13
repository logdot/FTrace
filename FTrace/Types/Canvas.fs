namespace FTrace.Types

open System
open System.Text.RegularExpressions
open Tuples

type Canvas =
    {
        Width:int
        Height:int
        Pixels:Tuplet[,]
    }

    static member create width height = 
        {
            Width=width
            Height=height
            Pixels=Array2D.create height width Colors.black
        }

module Canvas =
    let writePixel canvas x y color = Array2D.set canvas.Pixels y x color 
    let pixelAt canvas x y = Array2D.get canvas.Pixels y x

    let toPPM canvas =
        let clamp f =
            let rgbVal = 255.0 * f |> round
            Math.Clamp(int rgbVal, 0, 255) |> int

        let colorToRGB (c:Tuplet) =
            sprintf "%d %d %d" (clamp c.X) (clamp c.Y) (clamp c.Z)

        let generateRows (rgbs:seq<string>) =
            let row = String.Join(" ", rgbs)
            Regex.Replace(row, "[\s\S]{1,69}(?!\S)",
                          (fun m -> m.Value.TrimStart(' ') + "\n"))
                .TrimEnd('\n')

        let pixelsToString canvas =
            canvas.Pixels
            |> Array2D.map colorToRGB
            |> Seq.cast<string>
            |> Seq.chunkBySize canvas.Width
            |> Seq.map generateRows
            |> fun strings -> String.Join("\n", strings)

        let header =
            sprintf "P3\n%d %d\n255" canvas.Width canvas.Height

        let pixels = pixelsToString canvas

        sprintf "%s\n%s\n" header pixels


    let c = Canvas.create 10 20
    let ppm = toPPM c