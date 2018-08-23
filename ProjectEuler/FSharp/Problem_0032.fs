module Problem_0032

(*
7254は面白い性質を持っている. 39 × 186 = 7254と書け,
掛けられる数/掛ける数/積に1から9の数が1回ずつ出現する.
掛けられる数/掛ける数/積に1から9の数が1回ずつ出現するような
積の総和を求めよ.
HINT: いくつかの積は1通り以上の
掛けられる数/掛ける数/積
の組み合わせを持つが1回だけ数え上げよ.
*)

(*
2185824
1705 ms

2185824
960 ms
*)
let calc =
    let chkl = seq { 1..9 }
    seq { 1000..9999 }          // 1000～9999 の積を決めうち
    |> Seq.filter (fun c ->
        seq { 1..int(sqrt(float c)) }     // a(かけられる数) は 1～cの平方根まで
        |> Seq.filter (fun a ->
            let t = c.ToString()
            a.ToString().ToCharArray()
            |> Array.forall (fun x -> t.IndexOf(x) < 0)
        )
        |> Seq.filter(fun a -> c % a = 0)   // 割り切れる数かをチェック
        |> Seq.map(fun a -> sprintf "%d%d%d" a (c/a) c)
        |> Seq.exists(fun str ->
            chkl
            |> Seq.map(string)
            |> Seq.forall(fun x -> str.IndexOf(x) = str.LastIndexOf(x))
        )
    ) |> Seq.sum

let run() =
    calc
