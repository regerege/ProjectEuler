module Problem_0060
(*
Problem 60
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2060

素数3, 7, 109, 673は非凡な性質を持っている.
任意の2つの素数を任意の順で繋げると, また素数になっている.
 例えば, 7と109を用いると, 7109と1097の両方が素数である.
 これら4つの素数の和は792である.
 これは, このような性質をもつ4つの素数の組の和の中で最小である.

任意の2つの素数を繋げたときに別の素数が生成される, 5つの素数の組の和の中で最小のものを求めよ.

リアル: 00:00:19.195、CPU: 00:00:19.156、GC gen0: 908, gen1: 3, gen2: 1
val it : int list * int = ([13; 5197; 5701; 6733; 8389], 26033)
*)

#nowarn "40"
///素数シーケンス
let rec primes = 
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n + 2) else nextPrime(n + 2)
and isPrime n =
    if n >= 2 then
        primes 
        |> Seq.tryFind (fun x -> n % x = 0 || x * x > n)
        |> fun x -> x.Value * x.Value > n
    else false

//================================================================================
let isp a b = sprintf "%d%d" a b |> int |> isPrime
let isp2 a b = isp a b && isp b a
let rec intersect a b =
    if List.length a = List.length b then a = b
    elif List.length a < List.length b then
        match a,b with
        | [],_ -> true
        | _,[] -> false
        | x::xs,y::ys ->
            if x > y then false
            elif x = y then intersect xs ys
            else intersect a ys
    else false
(*
p1,[]
p2,[p1]
p3,[p2;p1]
...
Pn-1以下の素数の集合から条件に一致する素数のリストを作り、
nの値が大きいもの順から連結リスト的にチェックを行い、5つの素数を探し出す。
最初に見つけた5組の素数が最も小さい素数の集合となるはず。
*)
let part2() =
    // scan function
    // step1  [(2,[])]
    // step2  [(3,[]);(2,[])]
    // step3  [(5,[]);(3,[]);(2,[])]
    // step4  [(7,[3]);(5,[]);(3,[]);(2,[])]
    // step5  [(11,[??]);(7,[3]);(5,[]);(3,[]);(2,[])]
    let f l p1 =
        match l with
        | [] -> [p1,[]]
        | _ -> [p1,List.filter(isp2 p1) <| List.map fst l]@l
    // a : int list
    // b : (int * int list) lsit
    let rec choise = function
        | [] -> None
        | (p,ps)::xs ->
            List.filter (fst >> ((=))) xs
    primes
    |> Seq.scan f []
    |> Seq.skip 1
    |> Seq.filter (Seq.nth 0 >> snd >> List.length >> ((<=)4))
    |> Seq.take 10
    |> Seq.iter (printfn "%A")

//================================================================================
let part1() =
    let con a b = int <| sprintf "%d%d" a b
    let isp a b = isPrime (con a b) && isPrime (con b a)
(*
step1: 2.. [ (2,[2]); ]
step2: 3.. [ (2,[2]); (3,[3]); ]
step3: 5.. [ (2,[2]); (3,[3]); (5,[5]); ]
step4: 7.. [ (2,[2]); (3,[3;7;]); (5,[5]); (7,[3;7;]); ]  // リストに追加したkey分ループする必要あり
*)
    let edge =
        primes |> Seq.takeWhile ((>)10000)      // 1万以下の素数までを取得
        |> Seq.fold (fun l p ->
            // 分析関数的にエッジを取得
            let (l1,l2) =
                l |> List.map (fun (key,pl) ->
                    if (isp key p) then ((key,pl@[p]), Some key)
                    else ((key,pl), None))
                |> List.unzip
            List.append l1 [(p, l2 |> List.choose id)]) []
        // とりあえず確認用に上限を決める　（最終的には上限なしで行う）
//        |> Seq.skip 1500        // 1500回計算する。
//        |> Seq.head         // 事前に素数のエッジを結果を集める。
    // 最終手段・・・・！
    let intersect a b = List.filter (fun p -> List.exists((=)p) b) a
    seq {
        for (p1,pl1) in edge do
            let e1 = List.filter (fun (a,_) -> List.exists ((=)a) pl1 && p1 < a) <| edge.Tail
            for (p2,pl2) in e1 do
                let ipl2 = intersect pl1 pl2
                let e2 = List.filter (fun (a,_) -> List.exists ((=)a) ipl2 && p2 < a) <| e1.Tail
                for (p3,pl3) in e2 do
                    let ipl3 = intersect ipl2 pl3
                    let e3 = List.filter (fun (a,_) -> List.exists ((=)a) ipl3 && p3 < a) <| e2.Tail
                    for (p4,pl4) in e3 do
                        let ipl4 = intersect ipl3 pl4
                        let e4 = List.filter (fun (a,_) -> List.exists ((=)a) ipl4 && p4 < a) <| e3.Tail
                        for (p5,pl5) in e4 do
                            yield [p1;p2;p3;p4;p5]
    }
    |> Seq.head
    |> (fun l -> (l,List.sum l))

let run() =
    part1()
