module Problem_0041
#nowarn "40"

(*
n桁Pandigitalであるとは, 1からnまでの数を各桁に
1つずつもつこととする.
#下のリンク先にあるような数学的定義とは異なる
例えば2143は4桁Pandigital数であり, かつ素数である.
n桁（この問題の定義では9桁以下）Pandigitalな素数の
中で最大の数を答えよ.
*)
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
    |> Seq.map (List.fold (fun acc n -> acc + n.ToString()) "")
    |> Seq.map (int)

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

let run() =
    [2..9]
    |> Seq.map ((fun n -> [1..n]) >> permutation)
    |> Seq.map (Seq.filter isPrime)  // 条件
    |> Seq.fold (fun acc s -> Seq.append acc s) Seq.empty
    |> Seq.max
