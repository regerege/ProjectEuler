module Problem_0047_2

(*
予め領域を確保するのは不満あり。
限界値が分からない上での計算を行うべきかと思う為、没

リアル: 00:00:01.406、CPU: 00:00:01.372、GC gen0: 12, gen1: 1, gen2: 0
val it : int = 134043
*)

open System.Collections
let prime m =
    let primes = new BitArray(m+1, true)
    seq { 2..m }
    |> Seq.filter (fun n ->
        if (bigint n)*(bigint n) <= bigint m then
            if primes.[n] then
                for i in (n*n)..n..m do
                    primes.[i] <- false
        primes.[n])
    |> Seq.toArray
let primes = prime 200000

let pfset n =
    let rec pfs s c x pn =
        if primes.Length <= pn then printfn "%A" pn
        let p = primes.[pn]
        let (r,y) = (x%p),(x/p)
        if x < p then
            let s = if 0 < c then (Set.add (p,c) s) else s
            (n,s)
        elif r = 0 then pfs s (c+1) y pn
        elif c = 0 then pfs s 0 x (pn+1)
        else pfs (Set.add (p,c) s) 0 x (pn+1)
    pfs Set.empty 0 n 0

let calc n =
    Seq.initInfinite ((+)0)
    |> Seq.map pfset
//    |> Seq.map (fun s -> printfn "%A" s;s)
    |> Seq.windowed n   // シーケンスからn個の集合を作る
    |> Seq.filter(Array.forall(fun (_,s) -> s.Count = n))
    |> Seq.map(fun arr -> fst arr.[0])
    |> Seq.head

let run() = calc 4