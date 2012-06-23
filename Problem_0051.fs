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

リアル: 00:00:00.051、CPU: 00:00:00.046、GC gen0: 5, gen1: 1, gen2: 0
val it : string = "121313"
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

let searchprime len =
    primes
    |> Seq.skipWhile ((>=)56003)
    |> Seq.map string
    |> Seq.filter (fun s ->
        let checks =
            [
                '0', "123456789"
                '1', "023456789"
                '2', "013456789"
            ]
        let slen (c:char) = s.Split(c).Length
        // 0 or 1 or 2 でマスクした場合に3カ所以上置き換え出来る素数の場合に
        // リストからチェック用アイテムを取得
        let check = checks |> List.tryFind (fst >> slen >> (<=)3)
        match check with
        | Some (r,str) ->
            // 0~9の数字を置き換えて、置き換え後の数が素数のものだけをリストとして返す。
            let plist =
                seq {
                    for c in str -> int <| s.Replace(r,c)
                } |> Seq.filter isPrime
                |> Seq.toList
            // 0or1or2ですでに置き換えているので、それ以外の数字が素数のものが7つある場合かつ
            // 数字の桁数がすべて同じであることをチェック
            plist.Length = 7
            && plist
            |> List.map (fun n -> (string n).Length)
            |> Seq.groupBy (fun x -> x)
            |> (Seq.length >> (=)1)
        | None -> false
    )

let run() =
    searchprime 8       // 置き換えることで8つの素数
    |> Seq.head         // 最小の素数を取得
