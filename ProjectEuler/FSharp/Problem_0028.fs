﻿module Problem_0028

(*
1から初めて右方向に進み時計回りに数字を増やしていき, 5×5の螺旋が以下のように生成される:
21	22	23	24	25
20	7	8	9	10
19	6	1	2	11
18	5	4	3	12
17	16	15	14	13
両対角線上の数字の合計は101であることが確かめられる.
1001×1001の螺旋を同じ方法で生成したとき, 対角線上の数字の合計はいくつだろうか?
*)

// 1 3 5 7 9 13 17 21 24 .. 1001^2 まで足し込む
// maxは1以上の奇数でなければならない。
// 669171001 57ms
let calc max =
    let max = max * max
    Seq.initInfinite ((+)1)
    |> Seq.takeWhile ((>=)500)
    |> Seq.map(fun x -> 4*((2*x+1)*(2*x+1)) - (x*12))
    |> Seq.sum
    |> ((+)1)

let run () =
    calc 1001
