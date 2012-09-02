module Problem_0058

(*
1から始めて, 以下のように反時計回りに数字を並べていくと, 辺の長さが7の渦巻きが形成される.
65 64 63 62 61 60 59 58 57
66 37 36 35	34 33 32 31 56
67 38 17 16	15 14 13 30 55
68 39 18  5  4	3 12 29 54
69 40 19  6  1  2 11 28 53
70 41 20  7  8  9 10 27 52
71 42 21 22	23 24 25 26 51
72 43 44 45	46 47 48 49 50
73 74 75 76 77 78 79 80 81

面白いことに, 奇平方数が右下の対角線上に出現する.
もっと面白いことには, 対角線上の13個の数字のうち, 8個が素数である.
ここで割合は8/13 ≈ 62%である.
渦巻きに新しい層を付け加えよう. すると辺の長さが9の渦巻きが出来る. 以下, この操作を繰り返していく.
対角線上の素数の割合が10%未満に落ちる最初の辺の長さを求めよ.
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

let run() =
    Seq.initInfinite ((+)1)
    |> Seq.map (fun n -> let x = 2 * n + 1 in x * x,n)          // 等差数列？で求まる対角線の値を取得
    |> Seq.map (fun (x,n) -> [for y in 0..3 -> x - (2*n*y)])    // 4方向の対角線上の数値を配列で取得
    // 全体のカウント、素数のカウント をスキャンする。
    |> Seq.scan (fun (ac,pc) l ->
        let ac = ac + l.Length
        let pc = pc + (List.filter isPrime l).Length
        ac,pc
    ) (0,0)
    |> Seq.mapi (fun n a -> ((n-1)*2+1,a))                      // n週目を辺の長さに変換 2(n-1)+1
    |> Seq.filter (fun (_,(y,x)) -> let z = float x / float y in z < 0.1)   // 10%未満をフィルタリング
    |> Seq.head
    |> fst

(**
0: 9 25 49 81 => (1+n*2)^2
1: 7 21 43 73 => 9と7の差は2、25と21の差は4  => 上記式から - 2n すれば良さそう
2: 5 17 37 65 => 9と5の差は4（1の式の2倍）、25と17の差は8（1の式の2倍） => 0の式から - 4n すれば良さそう
3: 3 13 31 57 => 9と3の差は6

上記の法則より
(2n+1)^2 - 2ny
で求まるっぽい

定義
n = 層
y = 対角線の大きい数値順？

範囲
0 < n
0 <= n <= 3

> run();;
> リアル: 00:00:00.823、CPU: 00:00:00.764、GC gen0: 93, gen1: 0, gen2: 0
val it : int = 26241
**)
