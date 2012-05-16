module Problem_0020

(*
【問題】
n × (n - 1) × ... × 3 × 2 × 1 を n! と表す。
100! の各桁の数字の合計を求めよ。
*)

// 通常パターン
// 648
// 59 ms
let calc =
    let fac n =
        seq { 2I..n } |> Seq.reduce (*)
    fac 100I
    |> (string) |> (fun s -> s.ToCharArray())
    |> Array.map ((string)>>(int))
    |> Array.sum

let run() = calc
