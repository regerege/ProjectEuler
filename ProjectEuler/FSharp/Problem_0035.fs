module Problem_0035

(*
197は巡回素数と呼ばれる.
桁を回転させたときに得られる数
197, 971, 719
が全て素数だからである.
100未満には巡回素数が13個ある:
2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, および97である.
100万未満の巡回素数は何個か?
*)

open System.Collections
let prime m =
    let primes = new BitArray(m+1, true)
    seq { 2..m }
    |> Seq.filter (fun n ->
        if (bigint n)*(bigint n) <= bigint m then
            if primes.[n] then
                for i in (n*n)..n..m do
                    primes.[i] <- false
        primes.[n])
    |> Seq.max |> ignore
    primes
(*
55
912 ms
*)
let calc n =
    // 素数リスト（インデックスに素数判定したい数を入れる）
    let primes = prime n
    // 素数チェック
    let isp n = primes.[n]
    // 数を各桁ごとに分解
    let d x = x.ToString().ToCharArray() |> Array.map(string) |> Array.toList
    // 配列文字(数)を数に戻す。
    let re l = l |> List.reduce(+) |> int
    // [1;2;3;4;] が与えられたら 
    // [2341; 3412; 4123;] を得るための巡回リスト取得関数
    let rec getlist acc l =
        match l with
        | [_] -> [2]
        | hd::tail ->
            if List.length acc = (l.Length-1) then acc
            else
                let t = tail@[hd]
                getlist (acc@[re t]) t
        | _ -> []

    // 2から最大値までチェックする。
    seq { 2..n }
    |> Seq.filter(isp)  // 素数であるかをいったんチェックする。
    // getlistで得られた巡回リストがすべて素数であることをチェックする。
    |> Seq.filter(d>>(getlist [])>>List.forall(isp))
    |> Seq.length

let run () =
    calc (1000000-1)
