module Problem_0054

(*
カードゲームのポーカーでは, 手札は5枚のカードからなりランク付けされている. 役を低い方から高い方へ順に並べると以下である.

　１．役無し: 一番値が大きいカード
　２．ワン・ペア: 同じ値のカードが2枚
　３．ツー・ペア: 2つの異なる値のペア
　４．スリーカード: 同じ値のカードが3枚
　５．ストレート: 5枚の連続する値のカード
　６．フラッシュ: 全てのカードが同じスート (注: スートとはダイヤ・ハート・クラブ/スペードというカードの絵柄のこと)
　７．フルハウス: スリーカードとペア
　８．フォーカード: 同じ値のカードが4枚
　９．ストレートフラッシュ: ストレートかつフラッシュ
１０．ロイヤルフラッシュ: 同じスートの10, J, Q, K, A
ここでカードの値は小さい方から2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, Aである. (訳注：データ中で10は'T'と表される)

もし2人のプレイヤーが同じ役の場合には, 役を構成する中で値が最も大きいカードによってランクが決まる: 例えば, 8のペアは5のペアより強い (下の例1を見よ). それでも同じランクの場合には (例えば, 両者ともQのペアの場合), 一番値が大きいカードによってランクが決まる (下の例4を見よ). 一番値が大きいカードが同じ場合には, 次に値が大きいカードが比べれられ, 以下同様にランクを決定する.

例:

プレイヤー1	プレイヤー2	勝者
1	5H 5C 6S 7S KD (5のペア)	2C 3S 8S 8D TD (8のペア)	プレイヤー2
2	5D 8C 9S JS AC (役無し, A)	2C 5C 7D 8S QH (役無し, Q)	プレイヤー1
3	2D 9C AS AH AC (Aのスリーカード)	3D 6D 7D TD QD (ダイヤのフラッシュ)	プレイヤー2
4	4D 6S 9H QH QC (Qのペア, 9)	3D 6D 7H QD QS (Qのペア, 7)	プレイヤー1
5	2H 2D 4C 4D 4S (4-2のフルハウス)	3C 3D 3S 9S 9D (3-9のフルハウス)	プレイヤー1
poker.txtには1000個のランダムな手札の組が含まれている. 各行は10枚のカードからなる (スペースで区切られている): 最初の5枚がプレイヤー1の手札であり, 残りの5枚がプレイヤー2の手札である. 以下のことを仮定してよい

全ての手札は正しい (使われない文字が出現しない. 同じカードは繰り返されない)
各プレイヤーの手札は特に決まった順に並んでいるわけではない
各勝負で勝敗は必ず決まる
1000回中プレイヤー1が勝つのは何回か?
*)
open System
open System.IO

let sjis = System.Text.Encoding.GetEncoding(932)
let path = @"D:\develop\develop\fsharp\ProjectEuler\_datas\Problem054_poker.txt"

let (|Card|Suit|None|) (n:int) =
    match n with
    | 83 -> Suit(20)        // スペード
    | 67 -> Suit(21)        // クラブ
    | 68 -> Suit(22)        // ダイヤ
    | 72 -> Suit(23)        // ハート
    | 65 -> Card(14)        // ナンバー
    | 84 -> Card(10)        // ナンバー
    | 74 -> Card(11)        // ナンバー
    | 81 -> Card(12)        // ナンバー
    | 75 -> Card(13)        // ナンバー
    | x ->
        if 50 <= x && x <= 57 then Card(x-48)      // ナンバー
        else None           // 判定の必要がない値
let trunCard = function
    | Card x | Suit x -> x
    | None -> 0

module Poker =
    let card5 (s:seq<int>) =
        // カードの配列、フラッシュ判定用カウント　をスキャン
        s |> Seq.scan (fun (cards,(h,sc),c) x ->
            let (cards,(h,sc),c) =
                if c = 10 then [],(0,0),0
                else cards,(h,sc),c
            if x <= 14 then cards@[x],(h,sc),c+1
            else cards,(if h = x || sc = 0 then x,sc+1 else 0,0),c+1
        ) ([],(0,0),0)
        |> Seq.filter (fun (_,_,c) -> c = 10)
        |> Seq.map (fun (cards,(_,sc),_) -> cards,sc=5)
        // カードの判定
    let sort (s:seq<int list * bool>) =
        s |> Seq.map (fun (cards,f) ->
            let cs = cards |> Seq.countBy id |> Seq.sortBy snd
            let cc = cs |> Seq.map snd |> Seq.toList |> List.rev
            cs
            |> Seq.groupBy snd
            |> Seq.map (
                snd >> Seq.sortBy fst
                >> Seq.collect (fun (x,c) -> Array.create c x |> Array.toSeq)
                >> Seq.toList >> List.rev)
            |> Seq.toList
            |> List.rev
            |> List.collect id
            |> (fun l -> l,f,cc)
        )
    let role (s:seq<int list * bool * int list>) =
        s |> Seq.map (fun (cards,f,cc) ->
            let n = cards.[4]
            let s = cards = [(n+4)..(-1)..n] || cards = 14::[(n+3)..(-1)..n]
            let score =
                match cc with
                | [4;1] -> 7        // フォーカード
                | [3;2] -> 6        // フルハウス
                | [3;_;_] -> 3      // スリーカード
                | [2;2;1] -> 2      // ツーペア
                | [2;1;1;1] -> 1    // 1ペア
                | _ ->
                    match (n,s,f) with
                    | (10,true,true) -> 9
                    | (_,true,true) -> 8
                    | (_,_,true) -> 5
                    | (_,true,_) -> 4
                    | _ -> 0
            let score = if score <= 3 && f then 5 else score
            score::cards
        )

let ReadCard f =
    use sr = new StreamReader(path,sjis)
    seq { while not(sr.EndOfStream) do yield sr.Read() |> trunCard }
    |> Seq.filter ((<=)1)
    |> Poker.card5          // 5枚組を判定＆比較用に変換
    |> Poker.sort           // カードの並び替え
    |> Poker.role           // 役判定
    // player1、player2のペア
    |> Seq.scan (fun (p1,p2) cards ->
        match (p1,p2) with
        | ([],[]) -> cards,[]
        | (_,[]) -> p1,cards
        | _ -> cards,[]
    ) ([],[])
    |> Seq.filter (fun (p1,p2) -> p1.Length = 6 && p2.Length = 6)
    // 比較
    |> Seq.map (fun (p1,p2) ->
        if p1 < p2 then (0,1)
        elif p1 > p2 then (1,0)
        else (0,0)
        |> fun a -> a
    ) |> f

let run() =
    ReadCard <| (fun s ->
        s |> Seq.map (fun (player1,player2) ->
            if player1 < player2 then (0,1)
            elif player1 > player2 then (1,0)
            else (0,0)
        ) |> Seq.reduce (fun (p1,p2) (acm1,acm2) -> p1+acm1,p2+acm2)
        |> (printfn "%A")
    )

(*
(376, 624)
リアル: 00:00:00.104、CPU: 00:00:00.093、GC gen0: 3, gen1: 3, gen2: 0
val it : unit = ()
*)
