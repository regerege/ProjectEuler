module Problem_0019

(*
【問題】
次の情報が与えられている。
1900年1月1日は月曜日である。
9月、4月、6月、11月は30日まであり、2月を除く他の月は31日まである。
2月は28日まであるが、うるう年のときは29日である。
うるう年は西暦が4で割り切れる年に起こる。
しかし、西暦が400で割り切れず100で割り切れる年はうるう年でない。
20世紀（1901年1月1日から2000年12月31日）で月の初めが日曜日に
なるのは何回あるか。
*)

/// 曜日のアクティブパターン
let (|Sun|Mon|Tue|Wed|Thu|Fri|Sat|) = function
    | 1 -> Sun  //日
    | 2 -> Mon  //月
    | 3 -> Tue  //火
    | 4 -> Wed  //水
    | 5 -> Thu  //木
    | 6 -> Fri  //金
    | _ -> Sat  //土
let inline add m d = d % m + 1
let addMonth = add 12
let addWeekDay = add 7
let inline getMaxDay y m =
    match m with
    | 4 | 6 | 9 | 11 -> 30
    | 2 ->
        if (y % 400 <> 0) && (y % 100 = 0) then 28
        elif y % 4 = 0 then 29
        else 28
    | _ -> 31
let addDate (y,m,d,w) =
    let a = getMaxDay y m
    let w = addWeekDay w
    let d = add a d
    let m = if d = 1 then addMonth m else m
    let y = if m = 1 && d = 1 then y+1 else y
    (y,m,d,w)

//171
//51 ms
let calc () =
    let rec count y m d w c =
        if (y,m,d) = (2000,12,2) then c
        else
            let c =
                if 1901 <= y then
                    if (d,w) = (1,1) then c+1 else c
                else c
            let (y,m,d,w) = addDate (y,m,d,w)
            count y m d w c
    count 1900 1 1 2 0

// 超楽なやり方
//171
//55ms~58ms
open System
let rec count (d : DateTime) c =
    let c = if d.DayOfWeek = DayOfWeek.Sunday then c+1 else c
    match (d.Year, d.Month) with
    | (2001, 1) -> c
    | _ -> count (d.AddMonths(1)) c

let run() =
    count (DateTime.Parse("1901/1/1")) 0
//    calc ()