module Problem_0005_Monado

(*
【問題】
2520 は 1 から 10 の数字の全ての整数で割り切れる数字であり、
そのような数字の中では最小の値である。
では、1 から 20 までの整数全てで割り切れる数字の中で
最小の値はいくらになるか。
A. 232792560
*)

type LCMBuilder() =
    let lcm x y =
        if x = 0I || y = 0I then 0I
        else x * y / bigint.GreatestCommonDivisor(x,y)

    //60ms~71ms
    //232792560
    /// リストの数字全ての最小公倍数を求める
    let lcms list =
        /// 最小公倍数
        let rec lcm' l acm =
            match l with
            | [] -> acm
            | hd::tail -> (lcm hd acm) |> lcm' tail
        match list with
        | [] -> 0I
        | hd::tail -> lcm' tail hd
    member this.Return v = lcms v
        
let lcm = LCMBuilder()

let run () =
    lcm { return [1I..20I] }
