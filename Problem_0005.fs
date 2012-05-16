module Problem_0005

(*
【問題】
2520 は 1 から 10 の数字の全ての整数で割り切れる数字であり、
そのような数字の中では最小の値である。
では、1 から 20 までの整数全てで割り切れる数字の中で
最小の値はいくらになるか。
A. 232792560
*)

// パターン3
// ユークリッドの互除法
let lcl zero list =
    let rec _gcd x y =
        let i = y % x
        if i = zero then x
        else _gcd i x

    let rec _lcl list gcd lcm =
        match list with
        | [] -> (gcd, lcm)
        | hd :: tail ->
            let x = _gcd gcd hd
            let y = lcm * (hd / x)
            _lcl tail x y

    let l = List.tail list
    let h = List.head list
    (_lcl l h h)

let lcm x y =
    if x = 0I || y = 0I then 0I
    else x * y / bigint.GreatestCommonDivisor(x,y)

//60ms~71ms
//232792560
/// リストの数字全ての最小公倍数を求める
let lcms list =
    /// 最小公倍数
    let rec lcm' l acm =
        match l with
        | [] -> acm
        | hd::tail -> (lcm hd acm) |> lcm' tail
    match list with
    | [] -> 0I
    | hd::tail -> lcm' tail hd

let run() =
    lcms [1I..20I]
//// パターン2 (超駄作)
//let calc inc list =
//    let ListMul list1 list2 =
//        List.zip list1 list2 |> List.map (fun (x,y) -> x*y)
//    let rec checkMul max i num =
//        let ret = num * i
//        if num <= (max - ret) then checkMul max (i+inc) num
//        else i
//    // 再帰関数 (倍数リストを加算する)
//    let rec check list2 =
//        let mullist = ListMul list list2
//        let max = mullist |> List.max
//        let list3 =
//            List.zip list list2
//            |> List.map (fun (n,i) -> checkMul max i n)
//        let list4 = list3 |> ListMul list
//        let sum = List.forall (fun x -> list4.Head = x) list4
//        if sum then list3
//        else
//            List.rev list3
//            |> (fun l -> (l.Head + inc) :: l.Tail)
//            |> List.rev
//            |> check
//    check [for x in list -> inc]
//
//let run() =
//    calc 1M [1M..20M];

//// パターン1
//let calc zero inc list =
//    let checkdiv l n = l |> List.forall (fun i -> n % i = zero)
//    let rec check n =
//        if n = zero then None
//        elif checkdiv list n then Some(n, zero)
//        else check (n+inc)
//    Seq.unfold check (List.max list)
//
//let run () =
//    calc 0M 1M [1M..20M]
//    |> Seq.min
