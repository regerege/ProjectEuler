module Problem_0050

(*
素数41は6つの連続する素数の和として表せる:
41 = 2 + 3 + 5 + 7 + 11 + 13.
100未満の素数を連続する素数の和で表したときにこれが最長になる.
同様に, 連続する素数の和で1000未満の素数を表したときに最長になるのは953で21項を持つ.
100万未満の素数を連続する素数の和で表したときに最長になるのはどの素数か?

リアル: 00:00:00.191、CPU: 00:00:00.187、GC gen0: 16, gen1: 1, gen2: 0
val it : int * int = (997651, 543)
*)

#nowarn "40"
let rec primes = 
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n + 2) else nextPrime(n + 2)
and isPrime n =
    if n >= 2 then
        primes 
        |> Seq.tryFind (fun x -> n % x = 0 || x * x > n)
        |> fun x -> x.Value * x.Value > n
    else false

let calc max =
    let arr =
        primes
        |> Seq.scan (fun (_,s) p -> p,s+p) (0,0)
        |> Seq.skip 1
        |> Seq.takeWhile (snd >> (>)max)
        |> Seq.map fst
        |> Seq.toArray
    arr
    |> Seq.map (fun p ->
        arr
        |> Seq.filter ((<)p)
        |> Seq.scan (fun (s,c) p -> s+p,c+1) (p,1)
        |> Seq.filter (fst >> isPrime)
        |> Seq.maxBy snd
    )
    |> Seq.maxBy snd

let run() = calc 1000000
