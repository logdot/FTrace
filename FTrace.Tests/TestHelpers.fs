module TestHelpers

open FTrace.Constants

let lines (s:string) = s.Split("\n")
let (.=.) x y =
    x - y < Epsilon