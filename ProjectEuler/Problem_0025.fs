module Problem_0025

(*
フィボナッチ数列は以下の漸化式で定義される:
Fn = Fn-1 + Fn-2, ただし F1 = 1, F2 = 1.
最初の12項は以下である.
F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
12番目の項, F12が3桁になる最初の項である.
1000桁になる最初の項の番号を答えよ.
*)

//12 74ms
//4782 273ms
let fib () =
    Seq.initInfinite ((+)1)
    |> Seq.scan (fun (a,b) n ->
        (b,(a+b))
    ) (0I,1I)
    |> Seq.map(fst)
    |> Seq.findIndex(string >> (fun s -> s.Length >= 1000))

let run () =
    fib ()
