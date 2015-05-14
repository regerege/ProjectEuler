﻿module Problem_0068
(*
下に示す図のようなものを"magic" 3-gon ringという. これは1～6の数字を当てはめて, 各列の数字の和が9となっている. これを例として説明する.

p_068_1.gif
外側のノードのうち一番小さいものの付いた列(例では4,3,2)から時計回りに回ってそれぞれ列の数字を3つ連ねて説明する. 例えば例のものは4,3,2; 6,2,1; 5,1,3という組で説明することができる.

1～6の数字を当てはめて, 各列の数字の和が等しくなるものは次の8通りある.

合計	組
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6
この組の各数字を連結して, 9桁の数字で表すことができる. 例えば, 上の図のものは4,3,2; 6,2,1; 5,1,3であるので432621513である.

さて, 下の図に1～10の数字を当てはめ, 各列の数字の和が等しくなる"magic" 5-gon ringを作って, それを表す16桁または17桁の数字のうち, 16桁のものの最大の数字を答えよ.

(注, 3つの場合の例を見ても分かる通り, 列の始まりの数字を比べた時一番小さい数字で始まる列から時計回りに繋げるという条件のもとで文字列を生成する必要があります. この条件下で最大となる数字を答えてください. )

Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : string = "6531031914842725"
*)

let calc n =
    let m = (n*n + n*n + (n*n+n)/2 + n)/n
    let rec f l p =
        match l,p with
        | [],_ | _,[] | [],[] -> l
        | a::_,x::xs -> f ([(m-(a+x));a;x;]@l) xs
    f [(m-(2*n+1));n;n+1] [(2*n)..(-1)..(n+2)]
    |> List.rev

let run() =
    calc 5
    |> Seq.map string |> Seq.reduce(+)

