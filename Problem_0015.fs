module Problem_0015

(*
【問題】
2 × 2 のマス目の左上からスタートした場合、引き返しなしで右下にいくルートは 6 つある。
では、20 × 20 のマス目ではいくつのルートがあるか。
A. 137846528820
59 ms
*)
let fac n =
    if n = 0I then 1I
    elif n < 0I  then 0I
    else [1I..n] |> List.reduce(*)
let calcPathway mass =
    let m,n = mass,mass*2I
    fac(n) / (fac(m)*fac(n-m))

let run() =
    calcPathway 20I
