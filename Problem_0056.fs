module Problem_0056

(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2056

Googol (10^100)は非常に大きな数である:
 1の後に0が100個続く.
 100^100は想像を絶する.
 1の後に0が200回続く.
 その大きさにも関わらず, 両者とも桁の和は1である.
a, b < 100について自然数a^bを考える. 桁の和の最大を答えよ.
*)

let run() =
    seq { 2..99 }
    |> Seq.collect (fun a -> Seq.init 100 ((+)1) |> Seq.map(fun b -> a,b))
    |> Seq.skipWhile (fst >> (%)10 >> (=)0)
    |> Seq.map(fun (a,b) ->
        bigint.Pow(bigint(a), b).ToString()
        |> Seq.map (string >> int) |> Seq.sum)
    |> Seq.max

(**
> run();;
リアル: 00:00:00.219、CPU: 00:00:00.187、GC gen0: 14, gen1: 0, gen2: 0
val it : int = 972
**)
