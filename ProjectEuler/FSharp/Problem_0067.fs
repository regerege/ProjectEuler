module Problem_0067
open System
open System.IO
let path = """D:\develop\github\ProjectEuler\datas\Problem067_triangle.txt"""

(*
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2067

以下の三角形の頂点から下まで移動するとき, その数値の合計の最大値は23になる.

3
7 4
2 4 6
8 5 9 3
この例では 3 + 7 + 4 + 9 = 23

100列の三角形を含んでいる15Kのテキストファイル triangle.txt (右クリックして, 『名前をつけてリンク先を保存』)の上から下まで最大合計を見つけてください.

注：これは, Problem 18のずっと難しいバージョンです. 
全部で299 通りの組み合わせがあるので, この問題を解決するためにすべてのルートをためすことは可能でありません！
あなたが毎秒1兆本の(1012)ルートをチェックすることができたとしても, 全てをチェックするために200億年以上かかるでしょう. 
解決するための効率的なアルゴリズムがあります. ;o)

Real: 00:00:00.002, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
val it : int = 7273
*)

let problem67() =
    File.ReadLines path
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map (fun s -> s.Split([|' '|]) |> Seq.map (string >> int) |> Seq.toList)
    |> Seq.reduce (fun a b ->
        let c =
            List.zip ([0]@a) (a@[0])
            |> List.map (fun (a,b) -> max a b)
        List.zip c b
        |> List.map(fun (a,b) -> a+b))
    |> List.max

let run() =
    problem67()
