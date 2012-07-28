module Problem_0055

let rev (x:bigint) = bigint.Parse(System.String((string x).ToCharArray() |> Array.rev))
let take x =
    Seq.init 49 id
    |> Seq.scan (fun acm _ -> acm + rev acm) x
    |> Seq.skip 1
let calc() =
    Seq.init 9999 ((+)1)
    |> Seq.map (fun x -> bigint(x))
    |> Seq.filter (fun x ->
        take x
        |> Seq.tryFind (fun x -> x = rev x)
        |> Option.isNone
    ) |> Seq.length

let run() = calc()

(*
> run();;
リアル: 00:00:00.332、CPU: 00:00:00.249、GC gen0: 22, gen1: 0, gen2: 0
val it : int = 249
*)
