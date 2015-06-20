module Problem_0073
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2073

nとdを正の整数として, 分数 n/d を考えよう. n<d かつ HCF(n,d)=1 のとき, 真既約分数と呼ぶ.
d ≤ 8 について既約分数を大きさ順に並べると, 以下を得る:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
1/3と1/2の間には3つの分数が存在することが分かる.
では, d ≤ 12,000 について真既約分数をソートした集合では, 1/3 と 1/2 の間に何個の分数があるか?


> part4();;
Real: 00:00:00.057, CPU: 00:00:00.062, GC gen0: 0, gen1: 0, gen2: 0
val it : int * int * int * int * int = (7295372, 5999, 11999, 1, 2)

> part2();;
Real: 02:30:25.467, CPU: 02:30:08.734, GC gen0: 2, gen1: 2, gen2: 2
val it : int = 7295372
*)

///////////////////////////////////////////////////////////////////////////////////
// 解けないので以下を参考にF#に移植
// http://www.mathblog.dk/project-euler-73-sorted-reduced-proper-fractions/
///////////////////////////////////////////////////////////////////////////////////

let farey3 m a b c d =
    let rec find n = if (n+1)%b = 0 then (n+1)/b,n else find (n-1)
    let e,f = find m
    let rec farey acm a1 b1 c1 d1 =
        if not (c1 = c && d1 = d) then
            let k = (m+b1)/d1
            farey (acm+1) c1 d1 (k*c1-a1) (k*d1-b1)
        else acm,a1,b1,c1,d1
    farey 0 a b e f
let part4() =
    farey3 12000 1 3 1 2

let part3() =
    let limit = 12000
    let mutable a = 1
    let mutable b = 3
    let mutable c = 4000
    let mutable d = 11999
    let mutable result = 0
    while (not(c = 1 && d = 2)) do
        result <- result + 1
        let k = (limit + b) / d
        let e = k * c - a
        let f = k * d - b
        a <- c
        b <- d
        c <- e
        d <- f
    result
///////////////////////////////////////////////////////////////////////////////////

open System
open System.Collections.Generic

let farey2 max =
    let mutable count = 0
    let list = new List<int>()
    list.Add(3)
    list.Add(2)
    while count < (list.Count-1) do
        count <- 0
        for p in (list.Count-1)..(-1)..1 do
            let n = list.[p] + list.[p-1]
            if n <= max then list.Insert(p,n)
            else count <- count + 1
    list

let part2() =
    farey2 12000
    |> fun a -> a.Count - 2

///////////////////////////////////////////////////////////////////////////////////

let farey max =
    let rec farey' n (s:int list) =
        if 0 < n then
//            [for i in 0..2..(List.length s -1) do
//                yield s.[i]
//                yield s.[i] + s.[i+1]
//                yield s.[i+1]]
            let l =
                [for i in 0..(s.Length-2) do
                    yield s.[i] + s.[i+1]
                    yield s.[i+1]]
//                s |> Seq.pairwise
//                |> Seq.collect(fun (a,b) -> seq[(a+b);(b);])
//                |> Seq.append (seq[(Seq.head s)])
//                |> Seq.toList
            farey' (n-1) ([s.[0]]@l)
        else s
    farey' max [3;2]

let part1() =
    let rec f n a b =
        if a + b <= 12000 then f (n+1) (a+b) (min a b)
        else n
    let count = f 0 3 2
    let c = farey count |> Seq.filter ((>=)12000) |> Seq.length
    c - 2

///////////////////////////////////////////////////////////////////////////////////
    
let run() =
    part4()
