module Problem_0061
(*
Problem0061
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2061

三角数, 四角数, 五角数, 六角数, 七角数, 八角数は多角数であり, それぞれ以下の式で生成される.

三角数	P3,n=n(n+1)/2	1, 3, 6, 10, 15, ...
四角数	P4,n=n^2	1, 4, 9, 16, 25, ...
五角数	P5,n=n(3n-1)/2	1, 5, 12, 22, 35, ...
六角数	P6,n=n(2n-1)	1, 6, 15, 28, 45, ...
七角数	P7,n=n(5n-3)/2	1, 7, 18, 34, 55, ...
八角数	P8,n=n(3n-2)	1, 8, 21, 40, 65, ...
3つの4桁の数の順番付きの集合 (8128, 2882, 8281) は以下の面白い性質を持つ.

この集合は巡回的である. 最後の数も含めて, 各数の後半2桁は次の数の前半2桁と一致する
それぞれ多角数である: 三角数 (P3,127=8128), 四角数 (P4,91=8281), 五角数 (P5,44=2882) がそれぞれ別の数字で集合に含まれている
4桁の数の組で上の2つの性質をもつはこの組だけである.
三角数, 四角数, 五角数, 六角数, 七角数, 八角数が全て表れる6つの巡回する4桁の数からなる唯一の順序集合の和を求めよ.
*)

/// 多角数の計算式配列
let pnfl =
    [
        fun n -> n * (n + 1) / 2
        fun n -> n * n
        fun n -> n * (3 * n - 1) / 2
        fun n -> n * (2 * n - 1)
        fun n -> n * (5 * n - 3) / 2
        fun n -> n * (3 * n - 2)
    ]

let run () =
    // 多角数の集合 (全要素351個)
    // [[(1035, (10, 35)); (1081, (10, 81)); (1128, (11, 28)); .. ]
    let s =
        pnfl |> List.mapi (fun i f ->
            Seq.initInfinite ((+)1)
            |> Seq.map (fun n -> f n)
            |> Seq.filter ((<=)1000)
            |> Seq.takeWhile ((>=)9999)
            |> Seq.map (fun n -> (i+3),n, (n / 100), (n - (n/100*100)))
            |> Seq.toList)
    let rec search al hh ht s2 =
        seq {
            if List.length al = (List.length pnfl) && hh = ht then
                yield al;
            else
                for (key,n,ab,cd) in s2 do
                    if al = [] then
                        yield! search ([n]) ab cd (s |> List.collect id |> List.filter (fun (k,_,_,_) -> k <> key))
                    elif List.length al < (List.length pnfl) && ht = ab then
                        yield! search (al@[n]) hh cd (s2 |> List.filter (fun (k,_,_,_) -> k <> key))
        }
    search [] 0 0 (s.Head)
    |> Seq.iter (printfn "%A")
