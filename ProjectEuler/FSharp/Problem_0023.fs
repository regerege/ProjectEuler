module Problem_0023

(*
【問題】
完全数とは, その数の真の約数の和がそれ自身と一致する数のことである.
たとえば, 28の真の約数の和は, 1 + 2 + 4 + 7 + 14 = 28であるので,
28は完全数である.
真の約数の和がその数よりも少ないものを不足数といい,
真の約数の和がその数よりも大きいものを過剰数と呼ぶ.
12は, 1+2+3+4+6=16となるので, 最小の過剰数である.
よって2つの過剰数の和で書ける最少の数は24である.
数学的な解析により, 28123より大きい任意の整数は2つの過剰数の和で
書けることが知られている. 2つの過剰数の和で表せない最大の数が
この上限よりも小さいことは分かっているのだが, この上限を減らすこと
が出来ていない.
2つの過剰数の和で書き表せない正の整数の総和を求めよ.
*)

/// 約数リスト
let inline getDivList n =
    let rec finddiv l i =
        if n <= i then l
        elif n%i=0 then
            finddiv (i::l) (i+1)
        else
            finddiv l (i+1)
    finddiv [] 1

// 過剰数チェック
let inline isAN n =
    let sum = List.sum(getDivList n)
    (n < sum)

// 4179871
// 1905 ms~~2025 ms
let calc () =
    let max = 28123
    let arr = [| 1..max |]
    let chkANs = [| for i in 0..max -> isAN i |]
    let calcANs = arr |> Array.filter (fun i -> chkANs.[i])
    // 1..28123 の値の元となる項が過剰数かをチェック
    arr
    |> Array.filter (fun n ->
        // a+b=n を計算する
        calcANs     // 過剰数リストを検索対象とする。
        |> Array.exists (fun a ->
            let b = n - a
            if a >= n then false
            else chkANs.[b]
            // 過剰数チェックリストから n-a を計算した b の値
            // が過剰数かどうかをチェック
        )
        |> (not)
    )
    |> Array.sum

let run () =
    calc ()



//// 累乗
//let factorial x n =
//    let rec f' s n2 =
//        if n2 <= 0 then s
//        else f' (s * x) (n2-1)
//    f' 1 n
//
//// 約数の総和
//let inline sd n =
//    seq { for i in 1..(n/2) -> if (n % i = 0) then i else 0 }
//    |> Seq.sum
//    |> ((+)n)
//    |> (fun x -> x < n)

////過剰リストを作成
//let an n =
//    seq { for x in 1..n -> sd(x) } |> Seq.toArray
////    |> Seq.filter ((<)0)
////    seq { 12..(28123-1) }
////    |> Seq.map (fun x -> (x,sd x))
////    |> Seq.filter (fun (a,b) -> a < b)
////    |> Seq.map (fst)
