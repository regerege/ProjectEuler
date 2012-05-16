module Problem_0036

(*
585 = 10010010012 (2進) は10進でも2進でも回文数である.
100万未満で10進でも2進でも回文数になるような数の総和を求めよ.
(注: 先頭に0を含めて回文にすることは許されない.)
*)

(*
872187
739 ms
*)
let calc() =
  let tchk (a,b) = a = b
  let fa (n:int) = System.Convert.ToString(n,2).ToCharArray()
  let chkrev arr = Array.zip arr (Array.rev arr) |> Array.forall (tchk)
  let chk n =
    (chkrev (n.ToString().ToCharArray()))
    && (chkrev (fa n))
  Seq.initInfinite ((+)0)
  |> Seq.takeWhile ((>)1000000)
  |> Seq.filter (chk)
  |> Seq.sum

let run () =
    calc()
