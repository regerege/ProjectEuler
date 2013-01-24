module Problem_0017

(*
【問題】
1 から 5 までの数字を英単語で書けば one, two, three, four, five であり、
全部で 3 + 3 + 5 + 4 + 4 = 19 の文字が使われている。
では 1 から 1000 (one thousand) までの数字をすべて英単語で書けば、
全部で何文字になるか。
注: 空白文字やハイフンを数えないこと。
例えば、342 (three hundred and forty-two) は 23 文字、
115 (one hundred and fifteen) は20文字と数える。なお、"and"
を使用するのは英国の慣習。
A. 21124
45 ms
*)

let rec numcount n =
    match n with
    | 1 | 2 | 6| 10 -> 3
    | 4 | 5 | 9 -> 4
    | 3 | 7 | 8 | 40 | 50 | 60 -> 5
    | 11 | 12 | 20 | 30 | 80 | 90 -> 6
    | 15 | 16 | 70 -> 7
    | 13 | 14 | 18 | 19 -> 8
    | 17 -> 9
    | x when (20 < x) && (x < 100) && (x % 10 <> 0) ->
        (numcount (x/10*10)) + (numcount (x-(x/10*10)))
    | x when (100 <= x) && (x < 1000) ->
        let a = (numcount (x/100))
        let b = (x-(x/100*100))
        if 0 < b then a + (7+3) + (numcount b)
        else a + 7
    | 1000 -> 11
    | _ -> 0

open System
let run() =
    seq { 1..1000 } |> Seq.map(numcount) |> Seq.sum