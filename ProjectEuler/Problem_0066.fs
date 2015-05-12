module Problem_0066
(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2066

次の形式の, 2次のディオファントス方程式を考えよう:
x2 - Dy2 = 1
たとえば D=13 のとき, x を最小にする解は 649^2 - 13×180^2 = 1 である.
D が平方数(square)のとき, 正整数のなかに解は存在しないと考えられる.
D = {2, 3, 5, 6, 7} に対して x を最小にする解は次のようになる:
3^2 - 2×2^2 = 1
2^2 - 3×1^2 = 1
9^2 - 5×4^2 = 1
5^2 - 6×2^2 = 1
8^2 - 7×3^2 = 1
したがって, D ≤ 7 に対して x を最小にする解を考えると, D=5 のとき x は最大である.
D ≤ 1000 に対する x を最小にする解で, x が最大になるような D の値を見つけよ.

Real: 00:00:00.039, CPU: 00:00:00.031, GC gen0: 2, gen1: 2, gen2: 0
val it : string = "(661,Some(16421658242965910275055840472270471049, 638728478116949861246791167518480580))"
*)

// square number of infinite sequence
let squares =
    Seq.unfold (fun x -> Some(x,x+1I)) 1I
    |> Seq.map (fun x -> x,x*x)
    |> Seq.cache
// the find of nearest positive integer to square number
let find n = squares |> Seq.takeWhile (snd >> (>=)n) |> Seq.max |> fst
// Continued Fraction
let contfrac n =
    let i = find n
    let rec cf m d =
        seq {
            let a = i+(-1I*m)
            let b = (n-(m*m)) / d
            let c = a / b
            let a2I = a % b - i
            yield c
            if 1I < b then yield! (cf a2I b)
        }
    if i*i = n || n <= 1I then []
    else cf (-i) 1I |> Seq.toList |> (fun l -> [i]@l)
// Pell's Equation
let pell l =
    match l with
    | [] -> None
    | x::xs ->
        let rec pell' = function
            | [] -> None
            | [x] -> Some x
            | (a1I,a2I,a3I,a4I)::(b1I,b2I,b3I,b4I)::xs ->
                pell' ([(a1I*b1I+a2I*b3I, a1I*b2I+a2I*b4I, a3I*b1I+a4I*b3I, a3I*b2I+a4I*b4I)]@xs)
        let len = l.Length
        let n = xs.Length
        let l2 =
            Seq.append l xs
            |> Seq.take ((n % 2) * len + len - 1)
            |> Seq.map (fun x -> (x,1I,1I,0I))
            |> Seq.toList
        match pell' l2 with
        | None -> None
        | Some(a1,b1,a2,b2) ->
            Some <| [(a1,a2);(b1,b2)].[n%2]
let run() =
    [1I..1000I]
    |> Seq.map (fun D -> D, contfrac D |> pell)
    |> Seq.filter (fun (_,a) -> a.IsSome)
    |> Seq.maxBy (snd >> Option.get >> fst)
