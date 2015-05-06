module Problem_0065
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2065

2の平方根は無限連分数として書くことができる.
無限連分数である √2 = [1;(2)] と書くことができるが, (2) は2が無限に繰り返されることを示す. 同様に, √23 = [4;(1,3,1,8)].
平方根の部分的な連分数の数列から良い有理近似が得られることが分かる.√2の近似分数について考えよう.
従って, √2の近似分数からなる数列の最初の10項は：
1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
もっとも驚くべきことに, 数学的に重要な定数,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
e の近似分数からなる数列の最初の10項は：
2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
10項目の近似分数の分子の桁を合計すると1+4+5+7=17である.
e についての連分数である近似分数の100項目の分子の桁の合計を求めよ.

Real: 00:00:00.002, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 272
*)

// 自然対数の底の正則無限連分数
//let ecf =
//    Seq.initInfinite (fun x -> bigint x + 1I)
//    |> Seq.map (fun n ->
//        if n = 1I then 2I
//        elif n % 3I = 0I then n/3I*2I
//        else 1I)
// 自然対数の底の正則無限連分数
let ecf =
    Seq.append (seq[2I])
        <| (Seq.unfold (fun a -> Some (a,a+2I)) 2I
            |> Seq.map (fun x -> [1I;x;1I;])
            |> Seq.collect id)

// 連分数リストを近似分数として計算する。
let convergent = function
    | [] -> (0I,0I)
    | [x] -> (x,1I)
    | [x;y] -> (x+y,1I)
    | head::tail ->
        let rec convergent' m = function
            | [] -> (0I,0I)
            | [d] -> (m,d)
            | d::i::xs -> convergent' d ([d*i+m]@xs)
        let (m,d) = convergent' 1I <| List.rev tail
        (d*head+m,d)
let run() =
    convergent (ecf |> Seq.take 100 |> Seq.toList)  // 100番目の近似分数を計算
    |> (fst >> string)                              // 分子を文字列に変換
    |> Seq.map (string >> int)                      // 各桁の文字を数字に変換
    |> Seq.sum
