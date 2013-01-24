module Problem_0027

(*
オイラーは以下の二次式を考案している:
n2 + n + 41.
この式は, nを0から39までの連続する整数としたときに40個の素数を生成する.
しかし, n = 40のとき402 + 40 + 41 = 40(40 + 1) + 41となり41で割り切れる.
また, n = 41のときは412 + 41 + 41であり明らかに41で割り切れる.
計算機を用いて, 二次式 n2 - 79n + 1601という式が発見できた.
これはn = 0 から 79 の連続する整数で素数を生成する.
係数の積は, -79 × 1601 で -126479である.
さて, |a| < 1000, |b| < 1000 として以下の二次式を考える (ここで|a|は絶対値):
n2 + an + b
n=0から始めて連続する整数で素数を生成したときに最長の長さとなる上の二次式の,
係数a, bの積を答えよ.
*)

/////////////////////////////////////////////////////////////////////////
// 総当たりバージョン
open System.Collections
let getPrimes (max : int64) =
    let primes = new BitArray(int(max+1L), true)
    seq { 2L .. max }
    |> Seq.iter (fun n ->
        let n2 = int n
        if primes.[n2] then
            for i in (n*n)..n..max do primes.[int i] <- false)
    [| for b in primes -> b |]
//-59231 1387 ms
let find () =
    let primes = getPrimes 5000L
    let loop = seq { for a in -999..999 do for b in -999..999 -> (a,b) }
    let f a b n = (n*n) + (a * n) + b
    let count f =
        let rec count' n =
            let c = f n
            if 5000 < c || c < 0 then 0
            elif primes.[c] then count' (n+1)
            else n
        count' 0
    loop
    |> Seq.map (fun (a,b) -> (f a b, (a,b)))     // 関数の集合体 2000x2000
    |> Seq.map (fun (f,ab) -> (count f, ab))
    |> Seq.maxBy (fst)
//    |> (snd)
//    |> (fun (a,b) -> a * b)

let test() =
    let primes = getPrimes 100000L
    let exp n = (n*n)+n+41
    seq { -1..81 }
    |> Seq.map(fun n -> (n,exp n))
    |> Seq.iter (fun (n,p) -> printfn "|%d|%d|%b|" n p primes.[p])
/////////////////////////////////////////////////////////////////////////

// -59231 51ms
let calc max =
    let max = float max
    let a x = -2 * x + 1
    let b x = (x*x) - x + 41
    let x i = (1. + sqrt (1. + -4. * (41. - i))) / 2.
    let c = x max |> (int)
    ((a c) * (b c))

let run () =
//    getPrimes 2000L
//    test()
//    find()        // 総当たり
    calc 1000       // 計算で求めた
