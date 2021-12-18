module CanvasTests

open Xunit
open FsUnit.Xunit

open FTrace.Types.Canvas
open FTrace.Types.Tuples
open TestHelpers

[<Fact>]
let ``Creating a canvas``() =
    let c = Canvas.create 10 20
    c.Width |> should equal 10
    c.Height |> should equal 20
    c.Pixels |> should equal (Array2D.create 20 10 Colors.black)

[<Fact>]
let ``Width and height are accessible``() =
    let width = 10
    let height = 20

    let canvas = Canvas.create width height
    let array2d = Array2D.create height width Colors.black

    should not' shouldFail <| pixelAt canvas (width-1) (height-1)
    

[<Fact>]
let ``Writing a pixel to a canvas``() =
    let canvas = Canvas.create 10 20
    let color = Color 1. 0. 0.

    writePixel canvas 2 3 color
    pixelAt canvas 2 3 |> should equal Colors.red

[<Fact>]
let ``Verify ppm header``() =
    let canvas = Canvas.create 5 3
    let ppm = toPPM canvas |> lines
    ppm.[0] |> should equal "P3"
    ppm.[1] |> should equal "5 3"
    ppm.[2] |> should equal "255"

[<Fact>]
let ``Verify pixel data``() =
    let canvas = Canvas.create 5 3
    let color1 = Color 1.5 0. 0.
    let color2 = Color 0. 0.5 0.
    let color3 = Color -0.5 0. 1.

    writePixel canvas 0 0 color1
    writePixel canvas 2 1 color2
    writePixel canvas 4 2 color3
    let ppm = toPPM canvas |> lines

    ppm.[3] |> should equal "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
    ppm.[4] |> should equal "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
    ppm.[5] |> should equal "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"

[<Fact>]
let ``Long lines of ppm are split``() =
    let color = Color 1. 0.8 0.6
    let width = 10
    let height = 2

    let canvas = { Width = width; Height = height; Pixels = Array2D.create height width color }

    let ppm = toPPM canvas |> lines

    ppm.[3] |> should equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    ppm.[4] |> should equal "153 255 204 153 255 204 153 255 204 153 255 204 153"
    ppm.[5] |> should equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    ppm.[6] |> should equal "153 255 204 153 255 204 153 255 204 153 255 204 153"

[<Fact>]
let ``PPM files are terminated by a newline character``() =
    let ppm = Canvas.create 5 3 |> toPPM
    ppm |> should endWith "\n"
 