module Problem_0059

(*
(訳者注: 文字コードの説明は適当です) 各文字はそれぞれ一意のコードに割り当てられている.
よく使われる標準としてASCII (American Standard Code for Information Interchange) がある.
ASCIIでは, 大文字A = 65, アスタリスク (*) = 42, 小文字k = 107というふうに割り当てられている.

モダンな暗号化の方法として,テキストファイルの各バイトをASCIIに変換し,
秘密鍵から計算された値とXORを取るという手法がある. XOR関数の良い点は,
暗号化に用いたのと同じ暗号化鍵でXORを取ると平文を復号できる点である.
65 XOR 42 = 107であり, 107 XOR 42 = 65である.

破られない暗号化のためには, 鍵は平文と同じ長さのランダムなバイト列でなければならない.
ユーザーは暗号文と暗号化鍵を別々の場所に保存する必要がある.
また, もし一方が失われると,暗号文を復号することは不可能になる.

悲しいかな, この手法はほとんどのユーザーにとって非現実的である.
そこで, 鍵の変わりにパスワードを用いる手法が用いられる.
パスワードが平文より短ければ (よくあることだが), パスワードは鍵として繰り返し用いられる.
この手法では,安全性を保つために十分長いパスワードを用いる必要があるが,
記憶するためにはある程度短くないといけない.

この問題での課題は簡単になっている.
暗号化鍵は3文字の小文字である.
cipher1.txtは暗号化されたASCIIのコードを含んでいる.
また,平文はよく用いられる英単語を含んでいる.
この暗号文を復号し,平文のASCIIでの値の和を求めよ.

> run();;
リアル: 00:00:00.007、CPU: 00:00:00.000、GC gen0: 0, gen1: 0, gen2: 0
val it : int = 46367
*)

open System.IO
open System.Text

let path = @"D:\develop\develop\fsharp\ProjectEuler\ProjectEuler\datas\Problem059_cipher1.txt"
// ファイルの読み込み
let fileseq f =
    use sr = new StreamReader(path, Encoding.GetEncoding(932))
    let s =
        seq {
            while not <| sr.EndOfStream do
                yield sr.Read()
        }
        |> Seq.scan(fun (z,_) x ->
            if 48 <= x && x <= 57 then (z*10+x-48),0
            else 0,z
        ) (0,0)
        |> Seq.filter (snd >> (<)0)
        |> Seq.scan (fun l (_,x) ->
            match l with
            | [_;_;_;] -> [x]
            | _ -> l@[x]
        ) []
        |> Seq.filter (List.length >> (<=)3)
        |> Seq.pairwise
        |> Seq.map (fun (x,y) ->
            List.zip x y
            |> List.map (fun (a,b) -> a ^^^ b)
            |> List.sum)
        |> Seq.sum
    f s

let run() = fileseq id

(*
open System
open System.IO

let encoded =
    use suckIn = new StreamReader @"D:\develop\develop\fsharp\ProjectEuler\ProjectEuler\datas\Problem059_cipher1.txt"
    suckIn.ReadLine() |> fun x -> x.Split(',') |> Array.map Int32.Parse |> Array.map char

let textChars = [' '; '.'; ','; ';'; '?'; '!'; '"'; '\''; '('; ')']
                @ ['A'..'Z'] @ ['a'..'z'] @ ['0'..'9'] |> set
let inline isTextChar c = textChars.Contains c

let problem059 () =
    seq { for i in ['a'..'z'] do for j in ['a'..'z'] do  for k in ['a'..'z'] do yield seq [i; j; k] }
    |> Seq.map (fun x -> Seq.collect id (Seq.initInfinite (fun _ -> x)))
    |> Seq.map (Seq.zip encoded)
    |> Seq.map (Seq.map (fun (x,y) -> char ((int x) ^^^ (int y))))
    |> Seq.find (Seq.forall isTextChar) |> Seq.fold (fun s x -> s + (int x)) 0
*)