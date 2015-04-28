module Problem_0062
(*
Problem 62
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2062

立方数 41063625 (345^3) は, 桁の順番を入れ替えると2つの立方数になる: 56623104 (384^3) と 66430125 (405^3) である.
41063625は, 立方数になるような桁の置換をちょうど3つもつ最小の立方数である.

立方数になるような桁の置換をちょうど5つもつ最小の立方数を求めよ.

リアル: 00:00:00.513、CPU: 00:00:00.515、GC gen0: 72, gen1: 72, gen2: 0
val it : Numerics.BigInteger = 127035954683
*)
let run() =
    Seq.unfold (fun (b,len,l,n) ->
        let n2 = n * n * n
        let s2 = n2.ToString()
        let len2 = s2.Length
        let sortn = (Seq.sort s2 |> Seq.toArray)
        let b2 = len <> len2
        let l2 = if b2 then [n2,sortn] else l@[n2,sortn]
        Some ((b2,len,l,n),(false,len2,l2,(n+1I)))) (false,0,[],0I)
    |> Seq.filter (fun (b,_,_,_) -> b)
    |> Seq.collect (fun (_,_,l,_) ->
        Seq.groupBy snd l
        |> Seq.map (snd >> Seq.map fst >> Seq.toList)
        |> Seq.filter (List.length >> (=)5))
    |> Seq.head
    |> List.head
