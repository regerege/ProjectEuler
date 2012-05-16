module Problem_0030

(*
驚くべきことに, 各桁を4乗した和が元の数と一致する数は3つしかない.
1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
ただし, 1=14は含まないものとする. この数たちの和は
1634 + 8208 + 9474 = 19316 である.
各桁を5乗した和が元の数と一致するような数の総和を求めよ.
*)

let inline (^) (x:int) n = bigint.Pow((bigint x),n) |> int
// (443839, [4150; 4151; 54748; 92727; 93084; 194979])
// 2180 ms
let find exp =
    let n = [| for x in 0..9 -> x^exp |]
    let max = [| for i in 0..exp -> "9" |] |> Array.reduce(+) |> int
    seq { for n in 0..max -> (string n).PadLeft(exp,'0') }
    |> Seq.map (fun s ->
        s.ToCharArray()
        |> Array.map (string>>int)
        |> Array.map (fun i -> n.[i])
        |> Array.sum
        |> (fun n -> n,s.TrimStart('0')))
    |> Seq.filter (fst>>(<=)2)
    |> Seq.filter (fun (a,b) -> (string a) = b)
    |> Seq.map(fst)
    |> Seq.toList
    |> (fun l -> List.sum l, l)

let run () =
    find 3
