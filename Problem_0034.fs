module Problem_0034

(*
145は面白い数である. 1! + 4! + 5! = 1 + 24 + 120 = 145となる.
各桁の数の階乗の和が自分自身と一致するような数の総和を求めよ.
注: 1! = 1 と 2! = 2 は総和に含めてはならない.
*)

(*
40730
4443 ms
*)
let calc() =
    // factorial 階乗
    let f n =
        if n <= 1 then 1
        else seq { 2..n } |> Seq.reduce(*)
    // 0から9までの階乗結果を求める。
    let tmp = [0..9] |> List.map (f)
    // 与えられた数を1桁ずつに分解し、
    // 分解した数の階乗結果の総和を求める。
    let dn x =
        x.ToString().ToCharArray()
        |> Array.map (string>>int)
        |> Array.map (fun x -> tmp.[x])
        |> Array.sum
    // 1と2を含めない最大桁数までの数の総和を求める。
    seq { tmp.[3]..(tmp.[9]*7) }
    // フィルタリング：dn関数と与えた数が一致すること
    |> Seq.filter (fun x -> dn x = x)
    |> Seq.sum

let run () =
    calc()
