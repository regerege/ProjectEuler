module Problem_0010

(*
【問題】
10以下の素数の和は2 + 3 + 5 + 7 = 17である.
200万以下の全ての素数の和を計算しなさい.
A. 142913828922L
694 ms
*)

// ぱくりコード
// http://blogs.msdn.com/b/mpeck/archive/2009/03/03/solving-problems-in-csharp-and-fsharp-part-1.aspx
/// エラトステネスの篩
open System.Collections
let getPrimes (max : int64) =
    let primes = new BitArray(int(max+1L), true)
    seq { 2L .. max }   // 自然数nまでの全ての数を素数かどうか篩うためのループ処理
    |> Seq.filter (fun n ->
        let n2 = int n
        if primes.[n2] then     // 素数リストがtrueの場合
            for i in (n*n)..n..max do primes.[int i] <- false   // (n*n)から始まるnの倍数をリストから取り除く
        primes.[n2])            // 素数リストを返す。
    // シーケンスが呼ばれるたびに、前回の素数リストを引き継ぐ

let run() =
    getPrimes 2000000L |> Seq.reduce((+))
