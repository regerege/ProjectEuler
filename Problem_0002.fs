module Problem_0002

(*
【問題】
フィボナッチ数列の項は前の2つの項の和である。
 最初の2項を 1, 2 とすれば、最初の10項は以下の通りである。
　「1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...」
数列の項の値が400万を超えない範囲で、偶数値の項の総和を求めよ。
*)

////49ms~56ms 4613732M
/////フィボナッチ関数
///// min: 初期値（型定義用）
///// max: フィボナッチ数の最大値
//let fib min max =
//    let rec fibRT = function
//        | [] | [_] -> fibRT (min :: _arg1)
//        | _ ->
//            if max < _arg1.Head then _arg1
//            else fibRT((_arg1.[0] + _arg1.[1]) :: _arg1)
//    fibRT []
//
//let run () =
//    fib 1M 4000000M
//        |> List.filter (fun x -> x % 2M = 0M)
//        |> List.fold (+) 0M

//52~53ms 4613732M
let fib n m =
    Seq.unfold (fun (n1, n2) ->
        if (n1 < m) then Some(n1, (n2, n1 + n2))
        else None) (n, n)
    |> Seq.filter (fun x -> x % 2M = 0M)
    |> Seq.fold (+) 0M

//無限シーケンス版フィボナッチ
//67~74 ms  4613732
let fibInfinity =
    Seq.initInfinite ((+)0)
    |> Seq.scan (fun (x,y) n -> (y,x+y)) (0I,1I)
    |> Seq.map (snd)

let run () =
//    fib 1M 89M
    fibInfinity
    |> Seq.takeWhile ((>=)4000000I)
    |> Seq.filter (fun x -> x%2I=0I)
    |> Seq.sum
