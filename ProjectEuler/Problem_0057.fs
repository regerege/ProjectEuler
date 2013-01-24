module Problem_0057

open FsCommons.Fractions
//open Fractions

(*
2の平方根は無限に続く連分数で表すことができる.

√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
最初の4回の繰り返しを展開すると以下が得られる.

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
次の3つの項は99/70, 239/169, 577/408である. 第8項は1393/985である. これは分子の桁数が分母の桁数を超える最初の例である.

最初の1000項を考えたとき, 分子の桁数が分母の桁数を超える項はいくつか?
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2057
*)

let ContinuedFractionSeq =
    let x = { n = 1I; d = 2I }
    Seq.unfold (fun x ->Some (x,1I / (2I + x))) x
let chkFranLen x = x.n.ToString().Length > x.d.ToString().Length
let run() =
    ContinuedFractionSeq
    |> Seq.mapi (fun i x ->
        let x = 1I + x
        ((i,x),chkFranLen x)
    )
    |> Seq.take (1000-1)
    |> Seq.filter snd
    |> Seq.length

(**
> run();;
> リアル: 00:00:01.917、CPU: 00:00:01.918、GC gen0: 171, gen1: 171, gen2: 0
val it : int = 153
> 
**)
