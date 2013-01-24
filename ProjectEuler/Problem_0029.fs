module Problem_0029

(*
2 ≤ a ≤ 5 と 2 ≤ b ≤ 5について, abを全て考えてみよう:
22=4, 23=8, 24=16, 25=32
32=9, 33=27, 34=81, 35=243
42=16, 43=64, 44=256, 45=1024
52=25, 53=125, 54=625, 55=3125
これらを小さい順に並べ, 同じ数を除いたとすると,
15個の項を得る:
4, 8, 9, 16, 25, 27, 32, 64, 81, 125
, 243, 256, 625, 1024, 3125
2 ≤ a ≤ 100, 2 ≤ b ≤ 100 で同じことをしたとき
いくつの異なる項が存在するか?
*)

//9183 77 ms
let count () =
    let mb = 100
    let ma = bigint mb
    seq {
        for b in 2..mb do
            for a in 2I..ma ->
//                (a,b,bigint.Pow(a,b))
                bigint.Pow(a,b)
    }
    |> Seq.distinct
    |> Seq.length
    // チェック
//    |> Seq.countBy (fun (_,_,c) -> c)
//    |> Seq.filter ((snd)>>(<=)2)
//    |> Seq.length
    // チェック2?
//    |> Seq.sortBy (fun (_,_,c) -> c)
//    |> Seq.map (fun (a,b,c) ->
//        (System.Convert.ToString a, System.Convert.ToString b, System.Convert.ToString c)
//    )
//    |> Seq.iter (printfn "%A")

let run () =
    count ()

(*
4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23
1,1,2,2,4,9,11,11,13.13,15,17,24,24,25,25,28,30,33,33
素数では重複の増加量が0となる？？
*)