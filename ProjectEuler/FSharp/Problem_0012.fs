module Problem_0012

(*
【問題】
三角数の数列は自然数の和で表わされ、7番目の三角数は 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28 である。 三角数の最初の10項は
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
となる。
最初の7項について、その約数を列挙すると、以下のとおり。
 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
これから、7番目の三角数である28は、6個以上の約数をもつ最初の三角数であることが分る。
では、501 個以上の約数をもつ最初の三角数はいくらか。
A. (576, 34359607296)
200 ms
*)

let divisor = function
    | [] -> 1I
    | l ->
        let dic = l |> Seq.distinct
        l |> Seq.countBy(fun p -> dic |> Seq.find((=)p))
        |> Seq.map(fun (p,c)->bigint(c+1))
        |> Seq.reduce((*))
let pf num =
    let rec check (p,n) =
        if n < p then None
        elif n % p = 0I then Some(p, (p, n/p))
        else check(p+1I, n)
    Seq.unfold check (2I, num)
    |> Seq.toList
    |> (divisor)
open System
// 三角数かどうか
let isTNum x =
    let isTNum' x' =
        let ret = ((sqrt(8.0*x'+1.0)-1.0)/2.0)
        (ret - Math.Floor(ret)) = 0.0
    isTNum' (double x)
// 完全数 2<nなら値を返す
let inline (^) x n = bigint.Pow(x,n)
let perfectNum n = (2I^(n-1))*((2I^n)-1I)
let find() =
    Seq.initInfinite(((+)1)>>(perfectNum))
    |> Seq.filter(isTNum)
    |> Seq.map(fun n -> (pf n, n))
    |> Seq.find(fun (d,n) -> d >= 501I)

let run() =
    find()
    

////////////////////////////////////////////////////////////////////////////////////
//// 没
//let sigma n = (n*(n+1I))/2I
//let divisor = function
//    | [] -> 1I
//    | l ->
//        let dic = l |> Seq.distinct
//        l |> Seq.countBy(fun p -> dic |> Seq.find((=)p))
//        |> Seq.map(fun (p,c)->bigint(c+1))
//        |> Seq.reduce((*))
//
//let pf zero inc init num =
//    let rec check (p,n) =
//        if n < p then None
//        elif n % p = zero then Some(p, (p, n/p))
//        else check(p+inc, n)
//    Seq.unfold check (init, num)
//    |> Seq.toList
//    |> (divisor)
//let find () =
//    let pf' = pf 0I 1I 2I
//    Seq.initInfinite ((decimal)>>((+)1M)>>(sigma)>>(pf'))
////    |> Seq.iter(printfn "%A")
//    |> Seq.find((<=)501I)
////////////////////////////////////////////////////////////////////////////////////
