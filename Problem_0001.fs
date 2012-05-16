module Problem_0001

(*
1,000 未満の 3 or 5 の倍数になっている数字の合計を求めよ。
*)
//let exp x y = (y % x = 0)
//let exp2 a = (exp 3 a) || (exp 5 a)
let run ()=
// 総和の公式を使用
    let n = 1000 - 1
    let a = 3 * (n/3) * (n/3+1) / 2     // 3の倍数の合計
    let b = 5 * (n/5) * (n/5+1) / 2     // 5の倍数の合計
    let c = 15 * (n/15) * (n/15+1) /2   // 3と5の最小公倍数の合計
    a + b - c
//▲ 3ms


//let siguma =
//    let calc x num = num % x = 0
//    Seq.initInfinite (fun index -> index + 1)
//    |> Seq.takeWhile (fun num -> num <= 1000)
//    |> Seq.filter (fun num -> calc 3 num || calc 5 num)
//    |> Seq.sum
////▲ 58ms


// F#っぽいけど遅い
//    [1..1000]
//    |> List.filter exp2
//    |> List.fold (+) 0
//▲ 7ms ~ 8ms

// 何だかイマイチ 
//    List.sum
//        <| [
//            for x in [1..1000] do
//                if (exp2 x) then
//                    yield x
//        ]
//▲ 4ms ~ 5ms
        
// 気に入らないので没
//    List.sum
//        <| (
//            [1..1000] |> List.filter exp2
//        )
//▲ 5ms ~ 7ms

// 並列計算
//    let arr = [| 1..1000 |]
//    let a, b = arr |> Array.Parallel.partition (exp 3)
//    let c, d = arr |> Array.Parallel.partition (exp 5)
//    (Array.sum a) + (Array.sum c)
//▲ 6ms~12ms