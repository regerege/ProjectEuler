module Problem_0069
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2069

オイラーのトーティエント関数, φ(n) [時々ファイ関数とも呼ばれる]は, n と互いに素な n 未満の数の数を定める. たとえば, 1, 2, 4, 5, 7, そして8はみな9未満で9と互いに素であり, φ(9)=6.
n	互いに素な数	φ(n)	n/φ(n)
2	1	1	2
3	1,2	2	1.5
4	1,3	2	2
5	1,2,3,4	4	1.25
6	1,5	2	3
7	1,2,3,4,5,6	6	1.1666...
8	1,3,5,7	4	2
9	1,2,4,5,7,8	6	1.5
10	1,3,7,9	4	2.5
n ≤ 10 では n/φ(n) の最大値は n=6 であることがわかる.
n ≤ 1,000,000で n/φ(n) が最大となる値を見つけよ.

> part1();;
Real: 00:15:15.157, CPU: 00:15:15.437, GC gen0: 119694, gen1: 66000, gen2: 1101
val it : int * int = (30030, 5760)
> part2();;
Real: 00:00:00.009, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0
val it : int * int = (510510, 92160)
*)

#nowarn "40"
///素数シーケンス
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
let part2() =
    primes
    |> Seq.scan(fun (c,(a,b)) n ->
        let a = a*n
        let b = b*(n-1)
        (if b = 0 then 0M else (decimal a / decimal b)),(a,b)
        ) (0M,(1,1))
    |> Seq.takeWhile (snd >> fst >> ((>=)1000000))
    |> Seq.maxBy fst
    |> snd

let pf ((n,pfl),pl) =
    let rec pf2 p n pfl pl =
        match n,pl with
        | _,[] | 1,_ -> pfl
        | _,x::xs ->
            match (n%x,n/x) with
            | 0,x2 -> pf2 x x2 (if x <> p then pfl@[x] else pfl) pl
            | _,_ -> pf2 p n pfl xs
    let l = pf2 1 n [] (pl@[n])
    let l2 = if List.length l = 1 then pl@[n] else pl
    let a = (n,l),l2
    let b = (n+1,l),l2
    Some (a,b)
let totient ((n:int),l) = n,List.fold (fun n x -> (n-1)*(n/x)) n l
let part1() =
    Seq.unfold pf ((1,[1]),[])
    |> Seq.take 1000000
    |> Seq.map (fst >> totient)
    |> Seq.maxBy (fun (n,x) -> bigint n / bigint x)

let run() =
    part1()
