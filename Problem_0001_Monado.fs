module Problem_0001_Monado

(*
1,000 未満の 3 or 5 の倍数になっている数字の合計を求めよ。
*)

// 参考サイト
// http://richardminerich.com/2011/02/the-road-to-functional-programming-in-f-from-imperative-to-computation-expressions/
type EulerBuilder() =
    member b.Yield(x) = if x%5=0 || x%3=0 then x else 0
    member b.Combine(x,y) = x + y
    member b.Zero() = 0
    member b.For(vals, f) =
        vals |> Seq.fold (fun s n -> (s + (f n))) (b.Zero())
    member b.Run v = v
let eb = new EulerBuilder()

type EulerBuilder2() =
    member b.Yield x = if x%5=0 || x%3=0 then x else 0
    member b.For(generated,f) =
        generated |> Seq.map(fun x -> f x)
    member b.Run (filtered:int seq) = filtered |> Seq.sum
let eb2 = new EulerBuilder2()

let run() =
    eb2 { for i in 1..999 -> i }