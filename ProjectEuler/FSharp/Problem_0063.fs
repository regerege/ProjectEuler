module Problem_0063
(*
Problem 63
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2063

5桁の数 16807 = 7^5 は自然数を5乗した数である. 同様に9桁の数 134217728 = 8^9 も自然数を9乗した数である.
自然数を n 乗して得られる n 桁の正整数は何個あるか?

> run();;
リアル: 00:00:00.000、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
val it : int = 49
*)

let run () =
    Seq.unfold (fun (a,x,n,d) ->
        if n < d then None
        else
            let (x2,n2) =
                if n = d then x,n+1
                else x+1I,1
            let a2 = Array.create n2 x2 |> Array.reduce (*)
            let d2 = a2.ToString().Length
            Some ((a,x,n,d),(a2,x2,n2,d2))) (1I,1I,1,1)
    |> Seq.filter (fun (_,_,n,d) -> d = n)
    |> Seq.length
