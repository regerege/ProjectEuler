module Problem_0004

(*
【問題】
左右どちらから読んでも同じ値になる数を回文数という。
2桁の数の積で表される回文数のうち、最大のものは
9009 = 91 × 99 である。
では、3桁の数の積で表される回文数のうち
最大のものはいくらになるか。
*)

//60ms~62ms
//698896
let calc =
//    let inc = Seq.init 900 (fun x -> x + 100);
    let inc = Seq.init 900 ((+)100);
    Seq.zip inc inc
    |> Seq.map (fun (x,y) -> x * y)
    |> Seq.filter (fun num ->
        let oring = num.ToString().ToCharArray()
        let rev = oring |> Array.rev
        oring = rev)

//575ms~582ms
//906609
/// 100から999までの数同士の積を求め、文字列に変換後に
/// 反転した文字列とを比較する。
let calc2 =
    seq { for x in 100..999 do for y in 100..999 -> x*y }
    |> Seq.filter (fun num ->
        let oring = num.ToString().ToCharArray()
        let rev = oring |> Array.rev
        oring = rev)

let run () =
    calc2 |> Seq.max
