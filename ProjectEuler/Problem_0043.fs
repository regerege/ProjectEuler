module Problem_0043

(*
数1406357289は0から9のPandigital数である (0から9が1度ずつ現れるので).
この数は部分語が面白い性質を持っている.
d1を1桁目, d2を2桁目の数とし, 以下順にdnを定義する.
この記法を用いると次のことが分かる.

d2d3d4=406は2で割り切れる
d3d4d5=063は3で割り切れる
d4d5d6=635は5で割り切れる
d5d6d7=357は7で割り切れる
d6d7d8=572は11で割り切れる
d7d8d9=728は13で割り切れる
d8d9d10=289は17で割り切れる

このような性質をもつ0から9のPandigital数の総和を求めよ.

リアル: 00:00:32.735、CPU: 00:00:31.793、GC gen0: 2479, gen1: 4, gen2: 0
A. 16695334890L
24072 ms

http://infsharpmajor.wordpress.com/2011/11/26/project-euler-problem-43/
リアル: 00:00:10.674、CPU: 00:00:11.076、GC gen0: 278, gen1: 154, gen2: 4
val it : int64 = 16695334890L
*)

(*
10桁の数字
1234567890
*)
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
let plist = primes |> Seq.take 7 |> Seq.toList
let isp n = if 2 <= n && n <= 8 then plist.[n-2] else 1
let permutation list =
    let rec pseq acc (a:Set<int>) =
        seq {
            if a.Count <= 0 then
                yield acc
            else
                for b in a do
                    let c = Set.remove b a
                    yield! pseq (acc@[b]) c
        }
    pseq [] <| set { 0 .. (List.length list - 1) }
    |> Seq.map (List.fold (fun acc i -> acc@[list.[i]]) [])

let calc() =
    // 順列のシーケンスを取得
    "1234567890"
    |> Seq.toList
    |> permutation
    // 0～9の10桁のPandigital数のみを許可する。
    |> Seq.filter(fun l -> l.Head <> '0')
    |> Seq.filter(fun l ->
        // 3桁ずつ取得
        l |> Seq.windowed 3
        // 3桁のchar配列をint型に変換
        |> Seq.map(Array.map(string) >> Array.reduce(+) >> int)
        // 3桁の数字 mod 桁数番目の素数
        |> Seq.mapi (fun i n -> n % isp(i+1))
        // すべてが割り切れる事
        |> Seq.forall((=)0)
    )
    |> Seq.map(List.map(string) >> List.reduce(+) >> int64)
    |> Seq.sum

let run() = calc()
