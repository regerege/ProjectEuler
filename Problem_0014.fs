module Problem_0014

(*
【問題】
正の整数に以下の式で繰り返し生成する数列を定義する。
n → n/2 (n が偶数)
n → 3n + 1 (n が奇数)
13からはじめるとこの数列は以下のようになる。
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
13から1まで10個の項になる。
この数列はどのような数字からはじめても最終的には 1 になると
考えられているが、まだそのことは証明されていない(コラッツ問題)
さて、100万未満の数字の中でどの数字からはじめれば一番長い数列を
1,000,000
  999,999 ~ 1
生成するか。
注意: 数列の途中で100万以上になってもよい
A. (525, 837799)
xxx ms (ぱくりコードは 3秒未満)
*)

//超遅い自分のコード
//open System
//let inline even n = n >>> 1
//let inline odd n = (3I * n) + 1I
//let inline isEven n = (n &&& 1I)=0I
//let rec collatz l n =
//    if 1I < n then
//        if isEven n then
//            let x = (even n)
//            collatz (x::l) x
//        else
//            let y = (odd n)
//            collatz (y::l) y
//    else l
//let calc() =
////    seq { 999999I..(1I-2I)..1I }
//    seq { 999999I..(1I-2I)..500000I }
//    |> Seq.map(fun n ->
//        let a = collatz [n] n |> List.length
//        (a, n))
//    |> Seq.maxBy(fun (a,n) ->a)

////////////////////////////////////////////////////////////////
// http://stefanoricciardi.com/2010/11/23/project-euler-problem-14-in-f/
//open System.Collections.Generic
// 
//let is_odd x =
//    x % 2L = 1L
// 
//let rec collatz_sequence n (partial:int64 list) (cache: IDictionary<int64, int64 list>) =
//    if cache.ContainsKey(n) then
//        let result = partial @ cache.[n]
//        let sequence_starter = List.head result
//        if not(cache.ContainsKey(sequence_starter)) then
//            cache.Add(sequence_starter, result) |> ignore
//        result
//    else
//        match n with
//        | 1L ->
//            let result = partial @ [1L]
//            let sequence_starter = List.head result
//            cache.Add(sequence_starter, result) |> ignore
//            result
//        | n when is_odd n -> collatz_sequence ((3L*n) + 1L) (partial @ [n]) cache
//        | _ -> collatz_sequence (n/2L) (partial @ [n]) cache
// 
//let sequence_cache = new Dictionary<int64, int64 list>()
// 
//let calculate_sequence n =
//    collatz_sequence n [] sequence_cache
// 
//let max_sequence_lengths (max:int64) =
//    let all_sequences = [for i in 1L..max -> calculate_sequence i]
//    let lengths = List.map List.length all_sequences
//    let max = List.max lengths
//    let max_index = List.findIndex (fun (x:int64 list) -> x.Length = max) all_sequences
//    max_index + 1

////////////////////////////////////////////////////////////////
// 超早いぱくりコード
// http://netindonesia.net/blogs/hakimrie/archive/2008/03/29/f-vs-c-on-collatz-conjecture.aspx
let rec seq_length x n =
    match x with
    | x when x = 0L -> (n+1)
    | x when x = 1L -> seq_length 0L (n+1)
    | x when x%2L = 0L -> seq_length (x/2L) (n+1)
    | _ -> seq_length (3L*x + 1L) (n+1)

let rec loop i imax n =
    let n' = seq_length i 0
    let imax, n = if n' > n then i,n' else imax,n
    if i < 1000000L then loop(i+1L) imax n else imax

let run() =
//    calc()
//    max_sequence_lengths 999999L
    loop 1L 0L 0
