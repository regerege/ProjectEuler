open System
open System.Diagnostics

// モジュール名を変える事で別の問題の解答を取得出来る
let getAnswer =
    async {
        Problem_0044.run()
        |> printfn "%A"
        Async.CancelDefaultToken()
    }

let main() =
    // 非同期キャンセル情報
    let token = Async.DefaultCancellationToken
    let stopWatch = new Stopwatch()
    
    // ポーリングの定義
    let rec polling () =
        if not token.IsCancellationRequested then
            let msec = stopWatch.ElapsedMilliseconds
            if (msec <= 60L * 1000L) then
                polling ()
            else
                printfn "60秒をオーバーしました。"

    stopWatch.Start()
    
    // 非同期の実行
    Async.Start (getAnswer, token)

    // ポーリング監視
    polling ()

    // 非同期処理のキャンセル
    Async.CancelDefaultToken()

    stopWatch.Stop();
    printf "%d ms" stopWatch.ElapsedMilliseconds
    ()

main()