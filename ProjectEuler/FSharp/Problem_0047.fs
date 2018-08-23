module Problem_0047

(*
連続する2つの数がそれぞれ2つの異なる素因数を持つのは
14 = 2 × 7
15 = 3 × 5 の場合である.
同様に連続する3つの数がそれぞれ3つの異なる素因数を持つのは
644 = 22 × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19 の場合である.
連続する4つの数がそれぞれ4つの異なる素因数を持つ場合を考え,
連続する数の中で最小のものを答えよ.

リアル: 00:00:22.247、CPU: 00:00:22.027、GC gen0: 2728, gen1: 5, gen2: 1
val it : int = 134043
*)

#nowarn "40"
let rec primes =
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n+2) else nextPrime(n + 2)
and isPrime n =
    if 2 <= n then
        primes
        |> Seq.tryFind (fun x -> n % x = 0 || n < x * x)
        |> (fun x -> n < x.Value * x.Value)
    else false

let pftup p x =
    let rec pft c y=
        if y % p = 0 then pft (c+1) (y/p)
        else (y,c)
    pft 0 x
let pfset n =
    if 2 <= n then
        primes
        |> Seq.scan(fun (l,x) p ->
            let (r,c) = pftup p x
            if c = 0 then (l,x)
            else (l@[(p,c)],r)
        ) ([],n)
        |> Seq.find (snd >> (=)1)
        |> (fst >> set)
    else Set.empty
    |> (fun s -> (n,s))
let isPFCM n (arr: (int * Set<int*int>)[]) =
    let o = fst arr.[0]
    arr
    |> Array.collect(fun (x,s) ->
        arr.[x-o+1..]
        |> Array.map(fun b -> ((x,s),b))
    )
    |> Array.forall(fun ((_,x),(_,y)) ->
        if n = x.Count && n = y.Count then
            Set.intersect x y = Set.empty
        else false
    )

let calc n =
    Seq.initInfinite ((+)0)
    |> Seq.map pfset    // (x, xの素因数分解の集合) ←組
    |> Seq.windowed n   // シーケンスからn個の集合を作る
    |> Seq.filter (isPFCM n)
    |> Seq.map(fun arr -> fst arr.[0])
    |> Seq.head
let run() = calc 2
