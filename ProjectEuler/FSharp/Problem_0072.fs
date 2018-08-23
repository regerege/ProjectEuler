module Problem_0072
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2072

nとdを正の整数として, 分数 n/d を考えよう. n<d かつ HCF(n,d)=1 のとき, 真既約分数と呼ぶ.
d ≤ 8について真既約分数を大きさ順に並べると, 以下を得る:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
この集合は21個の要素をもつことが分かる.
d ≤ 1,000,000について, 真既約分数の集合は何個の要素を持つか?


> part4();;
Real: 00:00:00.504, CPU: 00:00:00.500, GC gen0: 21, gen1: 7, gen2: 1
val it : decimal = 303963552391M
> part1();;
Real: 00:04:23.854, CPU: 00:04:23.484, GC gen0: 107, gen1: 10, gen2: 1
val it : decimal = 303963552391M
*)

///////////////////////////////////////////////////////////////////////////////////
// くろとんさんのC++コードを移植
// いつかは immutable code で書いてみたい。書ければ。。

let part4 max =
    let s = [|0..max|]
    s.[1] <- 0
    for p in 2..max do
        if s.[p] = p then
            for i in [p..p..max] do
                s.[i] <- s.[i] - s.[i] / p
    s
    |> Seq.map (decimal)
    |> Seq.reduce (+)

///////////////////////////////////////////////////////////////////////////////////

let farey2 max =
    let rec farey p n t2 t1 =
        let len = String.length t1
        let p1 = t1.IndexOf(",",p+1)
        let p2 = t1.IndexOf(",",p1+1)
        let i1 = int(t1.[(p+1)..(p1-1)])
        let i2 = int(t1.[(p1+1)..(p2-1)])
        let t3 =
//            t2 + sprintf "%d,%d," i1 (i1+i2)
            if i1 + i2 <= max then
                t2 + sprintf "%d,%d," i1 (i1+i2)
            else t2 + sprintf "%d," i1
        if len = p2+1 then
            if 0 < n then
                farey -1 (n-1) "" (t3 + "1,")
            else t3 + "1,"
        else farey p1 n t3 t1
    farey -1 max "" "1,1,"
let part3() =
    let text = farey2 1000000
    let size = text |> Seq.fold (fun n c -> if c = ',' then n+1M else n) 0M
    size - 1M

///////////////////////////////////////////////////////////////////////////////////

let farey max =
    let rec farey' n s =
        if 0 < n then
            let l =
                s |> Seq.pairwise
                |> Seq.collect(fun ((a,b),(c,d)) ->
                    if b+d <= max then seq[(a+c,b+d);(c,d);]
                    else seq[(c,d);])
                |> Seq.append (seq[(Seq.head s)])
                |> Seq.toList
            farey' (n-1) l
        else s
    farey' (max-1) [(0,1);(1,1)]

let part2() =
    farey 20
    |> Seq.iter (snd >> printf "%d\t")
    printfn ""
//    |> Seq.fold (fun acm _ -> acm + 1M) 1M

///////////////////////////////////////////////////////////////////////////////////
let pf num =
    let rec f l p n =
        let p2 = if p = 2 then p+1 else p+2
        match n/p,n%p with
        | 1,0 -> l@[p]
        | a,0 -> f (l@[p]) p a
        | _,_ -> f l p2 n
    f [] 2 num
let totient n =
    let l = pf n
    match l with
    | [] -> 0
    | _ ->
        l |> Set.ofList
        |> Seq.fold (fun n x -> (x-1)*(n/x)) n
    
let part1() =
    Seq.unfold (fun a -> if 1 < a then Some(a,a-1) else None) 1000000
    |> Seq.map (totient >> decimal)
    |> Seq.sum
///////////////////////////////////////////////////////////////////////////////////
    
let run() =
    part4 12000
