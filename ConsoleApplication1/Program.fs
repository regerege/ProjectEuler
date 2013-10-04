module Program
open Commons

//リアル: 00:00:00.000、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : int = 233168
let Problem001() =
    seq { 1..999 } |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0) |> Seq.sum

//リアル: 00:00:00.005、CPU: 00:00:00.015、GC gen0: 0, gen1: 0, gen2: 0
//val it : System.Numerics.BigInteger = 4613732
let Problem002() =
    Seq.unfold (fun (a,b) -> Some ((a,b), (b,a+b))) (1I,1I)
    |> Seq.map snd
    |> Seq.takeWhile ((>=)4000000I)
    |> Seq.filter (fun x -> x % 2I = 0I)
    |> Seq.sum

//リアル: 00:00:00.007、CPU: 00:00:00.015、GC gen0: 1, gen1: 0, gen2: 0
//val it : System.Numerics.BigInteger = 6857
let Problem003() =
    let rec fac l p n =
        if n < p then l
        elif n%p = 0I then fac (l@[p]) p (n/p)
        else fac l (if p = 2I then 3I else p + 1I) n
    fac [] 2I 600851475143I
    |> List.max

//リアル: 00:00:00.000、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : int * (int * int) = (698896, (836, 836))
let Problem004() =
    let rec f a b =
        let c = a * b
        let d = string c |> Seq.toArray
        if d = (d |> Array.rev) then (c,(a,b))
        else f (if a = b then a-1 else a) (if a < b then b-1 else b)
    f 999 999

let Problem005() =
    let d n =
        let rec div l x i =
            if x < i then l
            elif x = i then l@[i]
            else
                let a,b = x/i,x%i
                if b = 0 then div (l@[i]) a i
                else div l x (if i = 2 then 3 else i+2)
        div [] n 2
    1

//リアル: 00:00:00.001、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : int = 25164150
let Problem006() =
    let s = seq { 1..100 }
    let a = s |> Seq.map (fun x -> x * x) |> Seq.sum
    let b = s |> Seq.sum |> fun x -> x * x
    abs (b - a)

//リアル: 00:00:00.000、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : int = 104759
let Problem007() =
    Commons.primes |> Seq.nth 10000

//リアル: 00:00:00.000、CPU: 00:00:00.015、GC gen0: 0, gen1: 0, gen2: 0
//val it : int = 40824
let Problem008() =
    let data = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    data |> Seq.map (string >> int) |> Seq.windowed 5 |> Seq.map (Seq.reduce (*)) |> Seq.max

//リアル: 00:00:07.185、CPU: 00:00:07.187、GC gen0: 157, gen1: 1, gen2: 1
//val it : int * int * int = (33, 544, 545)
let Problem009() =
    seq { for a in 1..999 do for b in 1..999 do for c in 1..999 -> a,b,c }
    |> Seq.map (fun (a,b,c) -> (a*a+b*b=c*c,a+b+c),(a,b,c))
    |> Seq.filter (fst >> fst)
    |> Seq.takeWhile (fst >> snd >> (>=)1000)
    |> Seq.maxBy (fst >> snd)

//リアル: 00:00:00.001、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : int = 199999
let Problem010() =
    primes
    |> Seq.takeWhile ((>=)200000)
    |> Seq.sum

//リアル: 00:00:00.006、CPU: 00:00:00.015、GC gen0: 0, gen1: 0, gen2: 0
//val it : int list * int = ([89; 94; 97; 87], 70600674)
let Problem011() =
    let text = @"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
    let arr = text.Split(' ') |> Array.map int
    // ■ 計算式,計算判定式 (右、下、右下、左下)
    let f1 x = x % 20 <= 16
    let f2 x = x / 20 <= 16
    let fl =
        [
            (fun x n -> x + n), f1
            (fun x n -> x + (n*20)), f2
            (fun x n -> x + (n*20) + n), fun x -> f1 x && f2 x
            (fun x n -> x + (n*20) - n), fun x -> 3 <= x % 20 && f2 x
        ]
    seq { 0..399 }
    |> Seq.collect (fun x ->
        fl |> Seq.map (fun (f,e) ->
            if e x then
                seq [0..3]
                |> Seq.map(fun n -> f x n)
                |> Seq.map(fun i -> arr.[i])
                |> Seq.toList
                |> fun l -> l,l |> List.reduce(*)
                |> Some
            else None)
        |> Seq.choose id)
    |> Seq.maxBy snd

//リアル: 00:00:14.913、CPU: 00:00:14.890、GC gen0: 622, gen1: 1, gen2: 0
//val it : decimal = 76576500M
// TODO: http://ideone.com/Wi5AQI (Haskell覚えたらまねてみる)
let Problem012() =
    let d n =
        let rec div l x i =
            if x < i then l
            elif x = i then l@[i]
            else
                let a,b = x/i,x%i
                if b = 0M then div (l@[i]) a i
                else div l x (if i = 2M then 3M else i+2M)
        div [] n 2M
        |> function
            | [] -> 0
            | l ->
                l |> Seq.groupBy id
                |> Seq.map (snd >> Seq.length >> (+)1)
                |> Seq.reduce(*)
    Seq.initInfinite (decimal >> (+)1M)
    |> Seq.map (fun n -> n * (n+1M) / 2M)
    |> Seq.filter (d >> (<=)500)
    |> Seq.head

//リアル: 00:00:00.004、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
//val it : string = "5537376230"
let Problem13() =
    Data.Problem13
    |> Seq.sum
    |> sprintf "%A"
    |> Seq.take 10 |> Seq.map string |> Seq.reduce (+)

//メモ化による高速化はいったん保留
let Problem14() =
    let ops = [ (fun n -> n/2L); (fun n -> 3L*n+1L); ]
    // (key, 長さ) list のメモ化
    Seq.init (1000000-1) ((+)0)
    |> Seq.skip 1
    |> Seq.map (fun x -> int64 x)
    |> Seq.scan (fun l n ->
        if l = [] then [1L,1]
        else
            let l2 =
                Seq.unfold (fun x -> Some (x,ops.[int(x%2L)] x)) n
                |> Seq.takeWhile ((<)1L)
                |> Seq.scan (fun (_,f) x ->
                    match List.tryFind (fst >> (=)x) l with
                    | None -> if f <> 0 then x,-1 else x,0
                    | Some (_,c) -> x,c) (0L,0)
                |> Seq.takeWhile (snd >> (=)0)
                |> Seq.toList
            let c = snd l2.[l2.Length - 1]
            l2 |> Seq.map fst
            |> Seq.mapi (fun i x -> x,i + 1 + c)
            |> Seq.toList
    ) []
    |> Seq.take 10
    |> Seq.iter (printfn "%A")

//http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2014


//リアル: 00:00:15.172、CPU: 00:00:15.765、GC gen0: 1446, gen1: 1374, gen2: 3
//val it : int * int list = (3500353, [9; 11; 17; 7; 20; 5; 21; 3; 24; 1])
let nmCoder37() =
    let list = [ 1; 3; 5; 7; 9; 11; 17; 20; 21; 24; ]
    let ops = [(+);(*)]
    permute list
    |> Seq.map(function
        | hd::tail ->
            tail
            |> List.fold (fun (op,(s,l)) x -> op^^^1,(ops.[op] x s, l@[x])) (0,(hd, [hd]))
            |> snd
        | _ -> (0,[]))
    |> Seq.maxBy fst

let Problem15() =
    let fac n = seq [(n/n)..n] |> Seq.reduce(*)
    let mass = 20I
    let m,n = mass,mass*2I
    fac n / (fac m * fac(n-m))

let Problem16() =
    string (bigint.Pow(2I,1000))
    |> Seq.map (string >> int)
    |> Seq.reduce(+)


let nmCoder46() =
    let ns = [ 26; 31; 13; 26; 19; 20 ]
    let ops = [ (+); (-); (*); ]
    let nops = [ 0..(ops.Length-1) ]
    let len = ns.Length - 1
    let rec s l =
        seq {
            if List.length l = len then
                yield l
            else
                for i in nops do
                    yield! s (l@[i])
        }
    let f l =
        let l2 = l |> List.map (fun i -> ops.[i])
        ns
        |> Seq.skip 1
        |> Seq.fold (fun (s,i) n -> ((l2.[i]) s n),i+1) (ns.[0],0)
        |> fst
    s []
    |> Seq.mapi (fun i l -> i,(l,f l))
    |> Seq.sortBy (snd >> snd >> abs >> (-)10000 >> abs)
    |> Seq.iter (printfn "%A")

