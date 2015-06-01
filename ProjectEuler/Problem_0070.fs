module Problem_0070
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2070

オイラーのトーティエント関数 φ(n) (ファイ関数とも呼ばれる) とは,
n 未満の正の整数で n と互いに素なものの個数を表す.
例えば, 1, 2, 4, 5, 7, 8 は9未満で9と互いに素であるので, φ(9) = 6 となる. 
1 は全ての正の整数と互いに素であるとみなされる. よって φ(1) = 1 である.

面白いことに, φ(87109)=79180 であり, 87109は79180を置換したものとなっている.

1 < n < 10^7 で φ(n) が n を置換したものになっているもののうち, n/φ(n) が最小となる n を求めよ.

> part2();;
Real: 00:00:00.832, CPU: 00:00:00.843, GC gen0: 230, gen1: 1, gen2: 0
val it : int * int = (8319823, 8313928)
*)

///////////////////////////////////////////////////////////////////////////////////

#nowarn "40"
let rec primes = 
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n + 2) else nextPrime(n + 2)
and isPrime n =
    if n >= 2 then
        primes 
        |> Seq.tryFind (fun x -> n % x = 0 || x * x > n)
        |> fun x -> x.Value * x.Value > n
    else false
// source code
// http://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }
(*
素数の集合 S
10,000,000 以下の素数を2つ用意する。
2つの素因数がでかければでかいほど結果は小さくなるので範囲を 2,000 ～ 5,000 とする。
（1,000から10,000までの範囲を扱っても、10^7以上の数が多く無駄な気がするのも理由の一つ）
366_C_2 = 66,795 の計算処理で結果が求まる。
*)
let part2() =
    let list =
        primes
        |> Seq.takeWhile ((>)5000)
        |> Seq.filter ((<)2000)
        |> Seq.toList
        |> List.rev
    combinations [] 2 list
    |> Seq.map (fun pl -> pl.[0]*pl.[1], (pl.[0]-1)*(pl.[1]-1))
    |> Seq.filter (fst >> ((>)10000000))
    |> Seq.filter (fun (a,b) ->
        let a = string a
        let b = string b
        if a.Length <> b.Length then false
        else
            let a = Seq.map string a |> Seq.sort |> Seq.reduce(+)
            let b = Seq.map string b |> Seq.sort |> Seq.reduce(+)
            a = b)
    |> Seq.minBy(fun (a,b) -> float a / float b)

///////////////////////////////////////////////////////////////////////////////////

let totient ((n:int),(l:int list)) = List.fold (fun n x -> (x-1)*(n/x)) n l
let fac n =
    let rec f n a l =
        if n = 1 then l
        else
            match (n%a),(n/a) with
            | 0,x -> f x a (l@[a])
            | _,_ -> f n (if a = 2 then a+1 else a+2) l
    f n 2 []
let part1() =
    Seq.initInfinite ((+)1)
    |> Seq.takeWhile((>=)10000000)
    |> Seq.map (fun n -> n,fac n)
    |> Seq.map (fun (n,f) -> n,(f,totient (n,f |> Set.ofList |> Seq.toList)))
    |> Seq.filter (fun (a,(_,b)) ->
        let a = string a
        let b = string b
        if a.Length <> b.Length then false
        else
            let a = Seq.map string a |> Seq.sort |> Seq.reduce(+)
            let b = Seq.map string b |> Seq.sort |> Seq.reduce(+)
            a = b)
    |> Seq.map (fun (a,(b,c)) -> a,c,(float a / float c),b.Length,b)
    |> Seq.minBy (fun (_,_,a,_,_) -> a)
    
///////////////////////////////////////////////////////////////////////////////////

let run() =
    part2()
