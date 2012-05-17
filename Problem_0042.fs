module Problem_0042

(*
三角数のn項は tn = ½n(n+1)で与えられる. 最初の10項は
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
である.
単語中のアルファベットを数値に変換した後に和をとる.
この和を「単語の値」と呼ぶことにする.
例えば SKY は 19 + 11 + 25 = 55 = t10である.
単語の値が三角数であるとき, その単語を三角語と呼ぶ.
16Kのテキストファイル words.txt 中に約2000語の英単語が記されている.
三角語はいくつあるか?
*)

let path = @"D:\develop\develop\fsharp\ProjectEuler\words.txt"
let sr = System.IO.File.OpenText path
let wordseq =
    let rec loop acc = seq {
        if sr.EndOfStream then sr.Dispose()
        else
            match sr.Read() with
            | 34 -> yield! loop acc
            | 44 ->
                yield acc
                yield! loop 0
            | x -> yield! loop (acc + x - 64)
    }
    loop 0

let istn x =
    let x = float x
    let n = (sqrt(8. * x + 1.) - 1.) / 2.
    (n - floor n) = 0. && 1. <= n

let run() =
    wordseq
    |> Seq.filter istn
    |> Seq.length
