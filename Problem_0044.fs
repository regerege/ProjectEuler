module Problem_0044

(*
五角数は Pn = n(3n-1)/2で生成される. 最初の10項は
1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
である.
P4 + P7 = 22 + 70 = 92 = P8である.
しかし差 70 - 22 = 48は五角数ではない.
五角数のペア PjとPkについて,差と和が五角数になるものを考える.
差を D = |Pk - Pj| と書く. 差 D の最小値を求めよ.
*)

let sqrtn n = n |> float |> sqrt |> int
let P n = n*(3*n-1)/2
let isP x = x = P ((1 + sqrtn(24*x + 1))/6)

let run() =
    Seq.initInfinite ((+)2)
    |> Seq.collect(fun k -> seq { 1 .. (k-1) } |> Seq.map(fun j -> (j,k)))
    |> Seq.filter(fun (j,k) ->
        let a,b = P j, P k
        let c,d = a+b, a-b |> abs
        isP c && isP d
    )
    |> Seq.map (fun (j,k) -> P j - P k |> abs)
    |> Seq.head
