module Problem_0045

(*
三角数, 五角数, 六角数は以下のように生成される.

三角数	Tn=n(n+1)/2	1, 3, 6, 10, 15, ...
五角数	Pn=n(3n-1)/2	1, 5, 12, 22, 35, ...
六角数	Hn=n(2n-1)	1, 6, 15, 28, 45, ...
T285 = P165 = H143 = 40755であることが分かる.

次の三角数かつ五角数かつ六角数な数を求めよ.

リアル: 00:00:00.010、CPU: 00:00:00.015、GC gen0: 0, gen1: 0, gen2: 0
val it : int64 = 1533776805L
*)

//let P p n = ((p - 2L)*n*n - (p - 4L)*n) / 2L
let H n =
    let n = int64 n
    (4L*n*n - 2L*n) / 2L

//三角数判定は不要（六角数の値は三角数とに含まれるため）
//let isT x =
//    let n = (sqrt(8*x + 1 |> float) - 1.) / 2.
//    (n - floor n) = 0. && 1. <= n
let isP x =
    let n = (sqrt(24L*x + 1L |> float) + 1.) / 6.
    (n - floor n) = 0. && 1. <= n
let isH x =
    let n = (sqrt(8L*x + 1L |> float) + 1.) / 4.
    (n - floor n) = 0. && 1. <= n

let run() =
    Seq.initInfinite ((+)(144))
    |> Seq.map H
    |> Seq.filter isP
    |> Seq.head
