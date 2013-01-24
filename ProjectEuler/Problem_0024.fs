module Problem_0024

(*
【問題】
順列とはモノの順番付きの並びのことである.
たとえば, 3124は数1, 2, 3, 4の一つの順列である.
すべての順列を数の大小でまたは辞書式に並べたものを辞書順と呼ぶ.
0と1と2の順列を辞書順に並べると
012 021 102 120 201 210
になる.
0,1,2,3,4,5,6,7,8,9からなる順列を辞書式に並べたときの100万番目を
答えよ
*)

let inline fac n =
    [1..n]
    |> List.fold (fun l i ->
        l@[i * l.[i-1]]
    ) [1]
let inline div x y = x/y,x%y
let inline exceptlist s l =
    let set1 = Set.ofList l
    Set.difference s set1 |> Set.toArray
// 278391560
// 68 ms
let find () =
    let set0 = set { 0..9 }
    let size = set0.Count - 1
    let except = exceptlist set0
    let faclist = fac size
    let rec combi acc n l =
        if n < 0 then l
        else
            let a,b = div acc (faclist.[n])
            let arr = except l
            combi b (n-1) (l@[arr.[a]])
    combi 1000000 size []

let run() =
    find ()
    |> List.map((+)48 >> char >> string)
    |> List.reduce(+)




////余興コード
//let find () =
//    let set1 = set { 0..9 }
//    set1 |> Set.map (fun s1 ->
//    let set2 = Set.remove s1 set1
//    set2 |> Set.map (fun s2 ->
//    let set3 = Set.remove s2 set2
//    set3 |> Set.map (fun s3 ->
//    let set4 = Set.remove s3 set3
//    set4 |> Set.map (fun s4 ->
//    let set5 = Set.remove s4 set4
//    set5 |> Set.map (fun s5 ->
//    let set6 = Set.remove s5 set5
//    set6 |> Set.map (fun s6 ->
//    let set7 = Set.remove s6 set6
//    set7 |> Set.map (fun s7 ->
//    let set8 = Set.remove s7 set7
//    set8 |> Set.map (fun s8 ->
//    let set9 = Set.remove s8 set8
//    set9 |> Set.map (fun s9 ->
//        let set10 = Set.remove s9 set9
//        let s10 = set10 |> Set.toList |> List.head
//        [ s1; s2; s3; s4; s5; s6; s7; s8; s9; s10; ]
//    )))))))))
//    |> Seq.iter (printfn "%A")

////総当たりコード
//let find () =
//    seq { 0L..9999999999L }
//    |> Seq.map (sprintf "%010d")
//    |> Seq.filter (fun s ->
//        s.ToCharArray()
//        |> Seq.countBy(fun x -> x)
//        |> Seq.exists(snd>>(<)1)
//        |> not
//    )
//    |> Seq.nth 1000000
