module Problem_0009

(*
【問題】
ピタゴラスの三つ組(ピタゴラスの定理を満たす自然数)とはa<b<cで
a² + b² = c²
を満たす数の組である.
例えば, 3² + 4² = 9 + 16 = 25 = 5²である.
a + b + c = 1000となるピタゴラスの三つ組が一つだけ存在する. このa,b,cの積を計算しなさい.
A. (200, 375, 425)
*)

let check (a,b,c) =
    (a + b + c = 1000)
    && (((*)a a) + ((*)b b) = ((*)c c))
/// 総当たり
let parm = seq {
    for a in 1..1000 do
        for b in a..1000 do
            let c = 1000 - a - b
            yield (a,b,c)
}

let run() =
    // 68 ms
    parm |> Seq.find check

//    // 75 ms
//    parm
//    |> Seq.skipWhile (check>>not)
//    |> Seq.head
