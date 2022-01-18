namespace FTrace

open System

module Constants =
    let Epsilon = 0.00001
    let PI = Math.PI

    let (./) x y = (x |> float) / y
    let (/.) x y = x / (y |> float)
    let (./.) x y = (x |> float) / (y |> float)