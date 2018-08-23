module Problem_0006_Monado

(*
【問題】
最初の10個の自然数について、その和の二乗と、二乗数の和は以下の通り。
1² + 2² + ... + 10² = 385
(1 + 2 + ... + 10)² = 3025
これらの数の差は 3025 - 385 = 2640 となる。
同様にして、最初の100個の自然数について和の二乗と二乗の和の差を求めよ。
A. 25164150
*)

type SumBuilder() =
    member this.Bind (v,f) = f <| seq{ 1..v }
    member this.For (v,f) = v |> Seq.map(fun x -> f x)
    member this.Yield v = (v,v*v)
    member this.Run v =
        v |> Seq.fold(fun (a,b) (x,y) -> (a+x,b+y)) (0,0)
        |> (fun (a,b) -> (a*a)-b)
let sum = SumBuilder()

let run () =
    sum {
        let! s = 100
        for x in s -> x
    }
