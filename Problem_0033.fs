module Problem_0033

(*
49/98は面白い分数である.
「分子と分母からそれぞれ9を取り除くと、
49/98 = 4/8 となり、
簡単な形にすることができる」
と経験の浅い数学者が誤って思い込んでしまうかもしれないからである.

我々は 30/50 = 3/5 のようなタイプは自明な例だとする.
このような分数のうち、
1より小さく分子・分母がともに2桁の数になるような自明でないものは、
4個ある.
その4個の分数の積が約分された形で与えられたとき, 分母の値を答えよ.
*)

(*
100
50 ms
*)
let calc =
    // 最大公約数を求める
    let gcd (a:int) (b:int) =
        let (A,B) = bigint a, bigint b
        int <| bigint.GreatestCommonDivisor(A,B)
    let f a b = a*10 + b
    // xn/yn = x/y を探す。
    seq {
      for i in 1..4 do
        for x in 1..9 do
          for y in 1..9 do
            for n in 1..9 ->
              let ret =
                match i with
                | 1 -> (f x n, f y n)
                | 2 -> (f x n, f n y)
                | 3 -> (f n x, f y n)
                | _ -> (f n x, f n y)
              (ret,(x,y),i)
    }
    |> Seq.filter (fun ((A,B),(x,y),i) ->
        let (a,b) = gcd A B, gcd x y
        A < B
        && A/a = x/b
        && B/a = y/b
    )
    |> Seq.fold (fun (a,b) ((A,B),(x,y),i) -> (a*A,b*B)) (1,1)
    |> (fun (A,B) -> B/(gcd A B))

let run () =
    calc
