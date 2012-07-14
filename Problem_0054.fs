﻿module Problem_0054

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

(*
１．入力値を数値に変換することを基本とする。
A -> 13  的な

2. 柄をどう扱うか
判別共用体にする？　-> クラスやオブジェクトが作られるので控えたい。
柄の枚数だけを判断出来れば良い、ただし3カード同士の比較が出来るようにする。

役が大きい順に並び替えて配列に入れる。
[4カード(14,13,12,11);なし(9);]
[ストレートフラッシュ(1,2,3,4,5);]
[フルハウス(スリーカード(3,2,1),ペアカード(14,13));]

ロイヤルストレートフラッシュ: 99999
ストレートフラッシュ

数字だけの最大値
14 * 4 + 13 = 69

役は100以上の数字を使う？
役なし                   2から14 (J,Q,K,Aが小さい順から11から14)
ワン・ペア               (13,13),10,9,7; => [113;10;9;7]
ツー・ペア               (10,10),(2,2),3; => [110102;3] ※並び順が重要 (1ペア目 + 100) * 100 + (2ペア目 + 100)
スリーカード             100000 + 数字;  (7,7,7),3,2 => [100007;3;2]
ストレート               (2,3,4,5,6) をどう数字に置き換えるか。
フラッシュ               Q(1,3,7,9,J) をどう数字に置き換えるか。
フルハウス               1
フォーカード             1
ストレートフラッシュ     1
ロイヤルフラッシュ       1

3. 
*)

let path = @"D:\develop\develop\fsharp\ProjectEuler\_datas\Problem054_poker.txt"



let run() =
    1