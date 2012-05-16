module Problem_0040

(*
正の整数を順に連結して得られる以下の10進の無理数を考える:
0.123456789101112131415161718192021...
小数第12位は1である.
dnで小数第n位の数を表す.
d1 × d10 × d100
 × d1000 × d10000
 × d100000 × d1000000
を求めよ.
*)

// 210
// 81 ms
let calc() =
    let s =
        Seq.cache <|
        seq {
            for n in 1..1000000 do
                for c in (string n) ->
                    (int c)-(int '0')
        }
    let s2 n = Seq.nth (n-1) s
    Seq.init 6 (fun x -> [for i = 1 to x do yield 10])
    |> Seq.map(fun l -> if l.Length <= 0 then 1 else List.reduce(*) l)
    |> Seq.map(s2)
    |> Seq.reduce(*)

let run () = calc()
