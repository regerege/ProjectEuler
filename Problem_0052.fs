module Problem_0052

(*
125874を2倍すると251748となる. これは元の数125874と同じ数を含む.
2x, 3x, 4x, 5x, 6xがxと同じ数を含むような最小の正整数xを求めよ.

リアル: 00:00:00.652、CPU: 00:00:00.655、GC gen0: 38, gen1: 0, gen2: 0
val it : int = 142857
*)

// 3秒かかる遅さ
//let magns = [ 2..6 ]
//let getcg s = s |> Seq.countBy (fun c -> c) |> Set.ofSeq
//let run() =
//    Seq.initInfinite ((+)1)
//    |> Seq.map (fun x -> string x,List.map ((*)x >> string) magns)
//    |> Seq.filter(fun (_,l) -> l.[0].Length = l.[4].Length)
//    |> Seq.filter (fun (s,l) ->
//        let sl = getcg s
//        l |> List.map getcg |> List.forall((=)sl)
//    )
//    |> Seq.head

// 最適化してみた
let run() =
    let magns = [ 2..6 ]
    let ss (s:string) = new System.String(s |> Seq.sort |> Seq.toArray)     //段違いにSetより早い
    Seq.initInfinite ((+)1)
    |> Seq.filter(fun x ->
        let y = string x
        let l = magns |> List.map ((*)x >> string)
        let b1 = y.Length = l.[4].Length
        // 桁数チェックすることで無駄な判定を省ける。
        if y.Length = l.[4].Length then
            // ソートした数字がすべて同じかどうか
            let (y,l) = ss y,List.map ss l
            List.forall((=)y) l
        else false
    )
    |> Seq.head
