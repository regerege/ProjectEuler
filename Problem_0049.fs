module Problem_0049

(*
項差3330の等差数列1487, 4817, 8147は次の2つの変わった性質を持つ。
(i)3つの項はそれぞれ素数である。
(ii)各項は他の項の置換で表される。
1, 2, 3桁の素数にはこのような性質を持った数列は存在しないが、
4桁の増加列にはもう1つ存在する。
それではこの数列の3つの項を連結した12桁の数を求めよ。

リアル: 00:00:00.995、CPU: 00:00:00.967、GC gen0: 46, gen1: 46, gen2: 0
[[set [1487; 4817; 8147]]; [set [2969; 6299; 9629]]]
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
    |> Seq.filter((<)1000)
    |> Seq.mapi (fun i p -> (i,p))
    |> Seq.toArray
let primes = prime 9999

let comb n =
    seq { 0..n }
    |> Seq.collect (fun x ->
        seq { (x+1)..n }
        |> Seq.map(fun y -> (x,y))
    )

let run() =
    let gs x = x |> string |> Seq.sort |> Seq.toList
    primes
    |> Seq.map(fun (i,p) ->
        let x = gs p
        primes.[i..]
        |> Seq.filter (snd >> gs >> (=)x)
        |> Seq.map snd
        |> Seq.toArray
    )
    |> Seq.filter (Array.length >> (<=)3)
    |> Seq.map (fun arr ->
        // 等差数列の組が３つの物を探す。
        comb (arr.Length-1)
        |> Seq.map(fun (x,y) ->
            let (x,y) = arr.[x],arr.[y]
            let z = y - x
            (x,y,z)
        )
        |> Seq.groupBy (fun (x,y,z) -> z)   // 項差が同じ物でグルーピング
        |> Seq.filter (snd >> Seq.length >> (=)2)
        |> Seq.map (fun (i,s) ->
            s
            |> Seq.fold (fun l (x,y,_) -> l@[x]@[y]) []
            |> Set.ofList
        )
        |> Seq.filter (Set.count >> (=)3)
        |> Seq.toList
    )
    |> Seq.filter (List.length >> (<)0)
    |> Seq.distinct
    |> Seq.toList
