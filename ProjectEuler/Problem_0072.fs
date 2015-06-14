module Problem_0072
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2072

nとdを正の整数として, 分数 n/d を考えよう. n<d かつ HCF(n,d)=1 のとき, 真既約分数と呼ぶ.
d ≤ 8について真既約分数を大きさ順に並べると, 以下を得る:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
この集合は21個の要素をもつことが分かる.
d ≤ 1,000,000について, 真既約分数の集合は何個の要素を持つか?

part1
303963552391M
*)

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
    farey 1000000
    |> Seq.fold (fun acm _ -> acm + 1M) 1M

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
    part2()
