module Problem_0039

(*
辺の長さが{a,b,c}と整数の3つ組である
直角三角形を考え,その周囲の長さをpとする.
p = 120のときには3つの解が存在する
{20,48,52}, {24,45,51}, {30,40,50}
p ≦ 1000 で解の数が最大になる p を求めよ.
*)

//(840, 8)
//4291 ms
let calc () =
    seq {
        for a in 3..(1000/3) do
        for b in (a+1)..((1000-a)/2) do
        for c in (b+1)..(1000-a-b) ->
            ((a*a+b*b=c*c),a+b+c)
    }
    |> Seq.filter(snd>>(>=)1000)
    |> Seq.filter(fst)
    |> Seq.countBy(snd)
    |> Seq.maxBy(snd)

let run () = calc()
