module Problem_0046

(*
Christian Goldbachは全ての奇合成数は平方数の2倍と素数の和で表せると予想した.
9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12
後に, この予想は誤りであることが分かった.
平方数の2倍と素数の和で表せない最小の奇合成数を答えよ.

リアル: 00:00:00.077、CPU: 00:00:00.078、GC gen0: 9, gen1: 1, gen2: 0
val it : int = 5777
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

let plist x =
    primes
    |> Seq.takeWhile((>=)x)
    |> Seq.toList
let slist x =
    let x = x / 2
    let x = x |> float |> sqrt |> int
    [ for a in x..(-1)..1 -> a * a * 2 ]

let run() =
    seq { 3..2..(System.Int32.MaxValue) }
    |> Seq.filter(isPrime >> not)
    |> Seq.filter(fun x ->
        let cg =
            slist x
            |> Seq.exists(fun y ->
                let z = x - y
                plist z
                |> Seq.exists((=)z)
            )
        not cg
    )
    |> Seq.head
