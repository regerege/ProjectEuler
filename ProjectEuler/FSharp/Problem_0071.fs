module Problem_0071
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2071

nとdを正の整数として, 分数 n/d を考えよう. n<d かつ HCF(n,d)=1 のとき, 真既約分数と呼ぶ.
d ≤ 8について既約分数を大きさ順に並べると, 以下を得る:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
3/7のすぐ左の分数は2/5である.
d ≤ 1,000,000について真既約分数を大きさ順に並べたとき, 3/7のすぐ左の分数の分子を求めよ.

> part1();;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int * int = (428570, 999997)
*)

// |a, c|  |2,3|
// |b, d|  |5,7|
// Farey sequence
let rec calc a b c d =
    let e,f = a+c,b+d
    if f <= 1000000 then calc e f c d
    else a,b
    
let part1() =
    calc 2 5 3 7
    
let run() =
    part1()
