module Problem_0003

(*
【問題】
13195 の素因数は 5、7、13、29 である。
600851475143 の素因数のうち最大のものを求めよ。
*)

// 58ms~62ms
// [71M; 839M; 1471M; 6857M]
/// 素因数分解関数
let pf zero inc init num =
    let rec check (p,n) =
        if n < p then None
        elif n % p = zero then Some(p, (p, n/p))
        else check(p+inc, n)
    Seq.unfold check ((*素数*)init, (*現在の数字*)num)

let run() =
    pf 0M 1M 2M 220M
    |> Seq.toList

////[6857M; 1471M; 839M; 71M]
///// 素因数分解関数
//let pf num =
//    let rec pfRT a b p list num =
//        if p <= num then
//            if num % p = a then
//                pfRT a b p (p :: list) (num / p)
//            else
//                pfRT a b (p + b) list num
//        else
//            list
//    pfRT 0M 1M 2M [] num
//    
//let run() =
//    pf 38M
