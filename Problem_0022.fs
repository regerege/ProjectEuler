module Problem_0022

(*
【問題】
5000個以上の名前が書かれている46Kのテキストファイルnames.txtを
用いる.
まずアルファベット順にソートせよ.
のち,各名前についてアルファベットに値を割り振り,
リスト中の出現順の数と掛け合わせることで,名前のスコアを計算する.
たとえば,リストがアルファベット順にソートされているとすると,
COLINはリストの938番目にある.
またCOLINは3 + 15 + 12 + 9 + 14 = 53という値を持つ.
よってCOLINは938 × 53 = 49714というスコアを持つ.
ファイル中の全名前のスコアの合計を求めよ.
*)

open System.IO
let read () =
    let path = "names.txt"
    seq {
        use sr = new StreamReader(path)
        while not sr.EndOfStream do
            yield sr.Read()
        yield int ','
    }

//871198282
//390ms
let calc () =
    read()
    |> Seq.fold (fun (l,s,a) n ->
        if int '"' = n then (l,s,a)
        elif int ',' = n then (l@[(s,a)],"",0)
        else
            let c = string (char n)
            let s = s + c
            let a = a + (n-64)
            (l,s,a)
    ) ([],"",0)
    |> (fun (l,_,_) -> l)
    |> Seq.sortBy (fst)
    |> Seq.mapi (fun i (s,a) -> (s,a*(i+1)))
    |> Seq.sumBy (snd)

let run() =
    calc ()
