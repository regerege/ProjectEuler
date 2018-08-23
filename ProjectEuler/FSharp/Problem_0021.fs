module Problem_0021

(*
【問題】
d(n)をnの真の約数の和と定義する。
（真の約数とはn以外の約数のことである。）
もし、d(a) = b かつ d(b) = a（a ≠ b）を満たすとき、
aとbは友愛数（親和数）であるという。
例えば、220の約数は1,2,4,5,10,11,20,22,44,55,110なので
d(220) = 284である。
また、284の約数は1, 2, 4, 71, 142なのでd(284) = 220である。
それでは10000未満の友愛数の合計を求めよ。
*)

(*
■アプローチ１
1から順番に約数の和を計算していき10000未満までのリストを作る。
リストから友愛数をfindしていきリストを作る。
リストを合計する。
*)

/// 素因数分解
let pf num =
    let rec check (p,n) =
        if n < p then None
        elif n % p = 0 then Some(p, (p, n/p))
        else check(p+1, n)
    Seq.unfold check (1, num)
    |> Seq.toList
/// sum divisor (約数の総和)
/// l 素因数リスト
let sd l =
    l |> List.fold(fun (y,c,s,s2,s3) x ->
        let c = if y = x then c+1 else 1
        let s = if c = 1 then s * s3 else s
        let s2 = if c = 1 then x else s2 * x
        let s3 = if c = 1 then 1 + s2 else s3 + s2
        (x,c,s,s2,s3)
    ) (0,0,1,1,1)
    |> (fun (x,c,s,s2,s3) -> s * s3)

// [1; 2; 2; 5; 11]
// 1^0 + 1^1 + 2^0 + 2^1 + 2^2 + 5^0 + 5^1 + 11^0 + 11
// ↑を fold で再現
// x リストから取得した値、xを使用後はyに渡す。
// y 前回の値
// c 指数カウント
// s 総合計（約数の総和となる値）
// s2 指数カウントに帯するyの累乗計 2^1,2^2,2^3 の値
// s3 同一項の指数ごとの数の総和 [2;2;2;3;5;] -> 2^0 + 2^1 + 2^2 + 2^3 を求める。

//31626
//1483 ms
let calc() =
    let list =
        Seq.init 10000 ((+)0)
        |> Seq.map (fun n -> n |> pf |> sd |> (fun x -> (n,x-n)))
        // 完全数でなく、かつ約数の総和が1より大きいもの
        |> Seq.filter (fun (a,b) -> (a<>b) && (1<b))
        |> Seq.toList
    let exists state = list |> List.exists ((=)state)
    list
    |> List.filter (fun (a,b) -> exists (b,a))
    |> List.sumBy (fst)

let run() =
    calc()
