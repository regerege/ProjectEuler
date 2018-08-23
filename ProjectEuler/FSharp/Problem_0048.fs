module Problem_0048

(*
次の式は、11 + 22 + 33 + ... + 1010 = 10405071317 である。
では、11 + 22 + 33 + ... + 10001000 の最後の10桁を求めよ。

リアル: 00:00:00.114、CPU: 00:00:00.093、GC gen0: 0, gen1: 0, gen2: 0
val it : string = "9110846700"
*)

let run() =
    seq { for x in 1I..1000I -> bigint.Pow(x,int x)}
    |> Seq.sum
    |> string
    |> (fun s -> s.Substring(s.Length - 10))
