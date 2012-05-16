module Problem_0031

(*
イギリスでは硬貨はポンド£とペンスpがあり，
一般的に流通している硬貨は以下の8種類である.
1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
以下の方法で£2を作ることが可能である．
1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
これらの硬貨を使って£2を作る方法は何通りあるか?
*)

// 156 68 ms
/// coinsの組み合わせ数を取得
let cc pennies =
    let coins = [| 200; 100; 50; 20; 10; 5; 2; 1; |]
    let coins2 = coins |> Array.rev |> (fun arr -> arr.[1..])
    let clen = coins.Length
    
    // 高額コインのリストを取得
    let rec mc c p l =
        let len = List.length l
        if len = clen then l
        elif p = 0 then mc c p (l@[0])
        else
            let t = coins.[len]
            let (a,b) = (p/t),(p%t)
            mc c b (l@[a])

    // coinの選択と必要金額の計算
    // 組み合わせ数を返す
    let rec calc (c:int) p l =
        match l with
        | [] -> calc (c+1) p (mc c p l)
        | _ ->
            let check = (List.sum l) = 200
            if check then c
            else
                let l2 =
                    let rl = List.rev l |> List.tail
                    let pos = rl |> List.findIndex ((<)0)
                    let len = rl.Length - 1
                    [ for i in len..(-1)..pos ->
                        let num =
                            if i = pos then rl.[i]-1
                            else rl.[i]
                        (num, coins2.[i] * num)
                    ]
                let (l3,lc) = List.unzip l2
                let p = pennies - (List.sum lc)
                calc (c+1) p (mc c p l3)
    calc 0 pennies []

let run() =
    cc 200

(*
[ 200; 100; 50; 20; 10; 5; 2; 1; ]
[   0;   1;  2;  0;  0; 0; 0; 0; ]
[   0;   1;  1;  2;  1; 0; 0; 0; ]
[   0;   1;  1;  2;  0; 2; 0; 0; ]
[   0;   1;  1;  2;  0; 1; 2; 1; ]
[   0;   1;  1;  2;  0; 1; 1; 3; ]
[   0;   1;  1;  2;  0; 1; 0; 5; ] // 使うコインが 1 のみになる
[   0;   1;  1;  2;  0; 0; 2; 6; ] // 使うコインが [ 2; 1; ] になる
[   0;   0;  0;  0;  0; 0; 0; 200; ]
// 配列操作（末尾の要素を抜いた要素を取得）
// 配列操作（配列の後ろから0を除いた1以上の数を-1する）
// coinの選択
coins.[list.length..] (6,7)
// ↓なら再帰処理を終了
list.[..-2]
|> List.forall ((=)0)
*)

(*
151
[0; 1; 1; 0; 0; 0; 0; 1]
[0; 0; 3; 0; 0; 0; 0; 1]
       3; 0; 0; 0; 0; 1]
*)