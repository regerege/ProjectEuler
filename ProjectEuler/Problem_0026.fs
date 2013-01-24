module Problem_0026

(*
単位分数とは分子が1の分数である。
分母が2から10の単位分数を10進数で表記すると次のようになる。
    1/2 = 0.5
    1/3 = 0.(3)
    1/4 = 0.25
    1/5 = 0.2
    1/6 = 0.1(6)
    1/7 = 0.(142857)
    1/8 = 0.125
    1/9 = 0.(1)
    1/10 = 0.1
0.1(6)は 0.166666... という数字であり、1桁の循環節を持つ。
1/7 の循環節は6桁ある。
d < 1000 なる 1/d の中で循環節が最も長くなるような d を求めよ。
*)

/// 循環節の長さを返す。
let rl n =
    if n = 0 then 0
    else
        let n = bigint n
        // 「n / (2^a * 5^b)」を求める。
        let rec d x n =
            let (a,b) = bigint.Divide(n,x),n%x
            if b = 0I then d x a
            else n
        let d25 = n |> (d 2I) |> (d 5I)
        if d25 = 1I then 0
        else
            // 10^r
            let pow10 = Seq.unfold (fun r -> Some(r,r*10I)) 10I
            pow10
            |> Seq.findIndex (fun a -> a % d25 = 1I)
            |> ((+)1)
//(983,982)
// 155ms
let run () =
    Seq.init 1000 ((+)0)
    |> Seq.map(fun d -> (d, rl d))
    |> Seq.maxBy(fun (a,b) -> b)
