module Problem_0037
#nowarn "40"

(*
3797は面白い性質を持っている.
まずそれ自身が素数であり,左から右に桁を除いたときに全て素数になっている
(3797, 797, 97, 7).
同様に右から左に桁を除いたときも全て素数である
(3797, 379, 37, 3).
右から切り詰めても左から切り詰めても
素数になるような素数は11個しかない.
総和を求めよ.
注: 2, 3, 5, 7を切り詰め可能な素数とは考えない.
*)

(*
748317
1549 ms
*)
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

let calc n =
  let d n =
    let arr = n.ToString().ToCharArray() |> Array.map(string)
    let len = arr.Length;
    [
      for p in 0..(len-2) do
        yield arr.[(p+1)..]
        yield arr.[..p]
    ] |> List.map(Array.reduce(+)>>int)
  Seq.initInfinite ((+)10)
  |> Seq.filter(isPrime)
  |> Seq.filter(d>>List.forall(isPrime))
  |> Seq.take n
  |> Seq.sum

let run () =
    calc 11
