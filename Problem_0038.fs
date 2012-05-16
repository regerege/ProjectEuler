module Problem_0038

(*
192を1, 2, 3で掛けてみよう.
   192 × 1 = 192
   192 × 2 = 384
   192 × 3 = 576
積を連結することで1から9のPandigital数
192384576
が得られる.
192384576を192と(1,2,3)の連結積と呼ぶ.
同じようにして,9を1,2,3,4,5と掛け連結することで
Pandigital数918273645が得られる.
これは9と(1,2,3,4,5)との連結積である.
整数と(1,2,...,n) (n > 1)との連結積として得られる
9桁のPandigital数の中で最大のものを答えよ.
*)

let calc() =
    let tmp = "123456789"
    let p n =
        let rec p' (s:string) c =
            if 2 <= c && 9 <= s.Length then s
            else p' (s+(n*c).ToString()) (c+1)
        decimal(p' "" 1)
    let ns n = System.String(n.ToString().ToCharArray() |> Array.sort)
    seq { 1..9999 }
    |> Seq.map(p)
    |> Seq.filter(ns>>(=)tmp)
    |> Seq.max

let run () =
    calc()
