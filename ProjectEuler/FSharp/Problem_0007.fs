module Problem_0007
#nowarn "40"

(*
【問題】
Problem 7
素数を小さい方から6つ並べると 2, 3, 5, 7, 11, 13 であり、
6番目の素数は 13 である。
10001 番目の素数を求めよ。
A. 104743
*)

// haskell
// sieve(p:ps) = p : sieve [n|n<-ps, n `mod` p /= 0]

// 863 ms
// エラトステネスの篩
let prime = ([],2) |> Seq.unfold (fun (l,n) ->
    let isp l n =
        l |> Seq.takeWhile (fun p->p*p<=n)
        |> Seq.exists ((%)n>>(=)0) |> not
    let p=Seq.initInfinite ((+)n) |> Seq.find (isp l)
    Some(p,(l@[p],p+1))
)

//104743
//5089 ms
let rec primes =
    seq {
        yield 2;
        yield! Seq.unfold nextPrime 3
    }
and nextPrime n =
    if isPrime n then Some(n, n+2)
    else nextPrime(n+2)
and isPrime n =
    if 2 <= n then
        primes
        |> Seq.tryFind (fun x -> n%x=0 || n<x*x)
        |> (fun x -> n < x.Value*x.Value)
    else false

let run() =
    primes |> Seq.item 10000
