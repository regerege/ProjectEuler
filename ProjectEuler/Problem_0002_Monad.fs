module Problem_0002_Monad

(*
【問題】
フィボナッチ数列の項は前の2つの項の和である。
 最初の2項を 1, 2 とすれば、最初の10項は以下の通りである。
　「1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...」
数列の項の値が400万を超えない範囲で、偶数値の項の総和を求めよ。
*)

type FibBuilder() =
    let fibSeq =
        Seq.initInfinite (fun x -> x)
        |> Seq.scan(fun (x,y) n -> (y,x+y)) (0,1)
        |> Seq.map(snd)
        |> Seq.cache
    member this.Yield (x:int) = if x%2=0 then x else 0
    member this.For (v,f) =
        let max = v |> Seq.max
        fibSeq
        |> Seq.takeWhile ((>=)max)
        |> Seq.map(fun x -> f x)
    member this.Run v = v |> Seq.sum
let fib = FibBuilder()

let run() =
    fib { for x in 0..(4000000-1) -> x }
