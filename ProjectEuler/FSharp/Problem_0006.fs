module Problem_0006

(*
【問題】
最初の10個の自然数について、その和の二乗と、二乗数の和は以下の通り。
1² + 2² + ... + 10² = 385
(1 + 2 + ... + 10)² = 3025
これらの数の差は 3025 - 385 = 2640 となる。
同様にして、最初の100個の自然数について和の二乗と二乗の和の差を求めよ。
A. 25164150
*)

// 46 ms
// 総和の公式を使用
let sigma k =
    (k * (k+1)) / 2
let sigma2 k =
    (k * (k+1) * (2*k + 1)) / 6
let calc k =
    let num = sigma k
    (num * num) - (sigma2 k)

////65 ms
//let calc k =
//    let list = [1..k]
//    List.zip list list
//    |> List.map (fun (x,y) -> (x,y*y))
//    |> List.unzip
//    |> (fun (a,b) ->
//        let sum = (a |> List.sum)
//        (sum * sum) - (b |> List.sum))

let run() =
    calc 100