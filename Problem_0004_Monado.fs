module Problem_0004_Monado

(*
【問題】
左右どちらから読んでも同じ値になる数を回文数という。
2桁の数の積で表される回文数のうち、最大のものは
9009 = 91 × 99 である。
では、3桁の数の積で表される回文数のうち
最大のものはいくらになるか。
*)

type PalindromeBuilder() =
    let (^^) x n = [ for a in 1..n -> x ] |> Seq.reduce(*)
    member this.Bind(v,f)=
        let s = seq { (10^^(v-1))..((10^^v)-1) }
        f <| Seq.zip s s
    member this.For(s,f) =
        s |> Seq.map(fun (a,b) -> a*b)
        |> Seq.filter(fun n ->
            let a = n.ToString().ToCharArray()
            let b = a |> Array.rev
            a = b
        ) |> Seq.map(fun n -> f n)
    member this.Yield v = v
    member this.Run v = v |> Seq.max
let pal = PalindromeBuilder()

let calc() =
    pal {
        let! a = 3
        for x in a -> x
    }

let run () = calc()