module Problem_0051

(*
*3の第1桁を置き換えることで, 13, 23, 43, 53, 73, 83という6つの素数が得られる.
56**3の第3桁と第4桁を同じ数で置き換ることを考えよう.
この5桁の数は7つの素数をもつ最初の例である:
56003, 56113, 56333, 56443, 56663, 56773, 56993.
よって, この族の最初の数である56003は, このような性質を持つ最小の素数である.
桁を同じ数で置き換えることで8つの素数が得られる最小の素数を求めよ.
(注:連続した桁でなくても良い)


※問題文の意味が分けがわからないので、とりあえず下記URLの人のコードの一部をぱくる
http://infsharpmajor.wordpress.com/2011/11/28/project-euler-problem-51/
*)

open System

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

let of3Digits0To2 (s: string) =
    let counts = [| (s.Split('0').Length - 1);
                    (s.Split('1').Length - 1);
                    (s.Split('2').Length - 1); |]
    if counts.[1] = 3 && s.Substring(5) = "1" then false
    else counts.[0] >= 3 || counts.[1] >= 3 || counts.[2] >= 3

let candidates =
    primes
    |> Seq.takeWhile ((>) 1000000)
    |> Seq.filter ((<) 100000)
    |> Seq.map string
    |> Seq.filter of3Digits0To2

let has7Transforms (s: string) =
    let variant =
        if s.Split('0').Length - 1 >= 3 then
           ("0",["1";"2";"3";"4";"5";"6";"7";"8";"9"])
        elif s.Split('1').Length - 1 >= 3 then
           ("1",["0";"2";"3";"4";"5";"6";"7";"8";"9"])
        else
           ("2",["0";"1";"3";"4";"5";"6";"7";"8";"9"])

    Seq.length
        (seq {
            for i in (snd variant) do
                let candidate = Int32.Parse(s.Replace((fst variant), i))
                if isPrime candidate && candidate >= 100000 then
                    yield candidate
              }) >= 7

let problem051 () =
    candidates
    |> Seq.find has7Transforms
