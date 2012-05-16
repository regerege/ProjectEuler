module Problem_0016

(*
【問題】
2^15 = 32768 であり、これの各数字の合計は 3 + 2 + 7 + 6 + 8 = 26 となる。
同様にして、2^1000 の各数字の合計を求めよ。
A. 1366
55 ms
*)

open System
let run() =
    let ret = 2I<<<(1000-1) |> string
    seq { for i in 0..(ret.Length-1) -> ret.[i] }
    |> Seq.map((string)>>(int))
    |> Seq.sum
