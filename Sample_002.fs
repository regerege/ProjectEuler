module Sample_002

(*
計算式ビルダ　サンプル
参考書： プログラミング F# (978-4-87311-466-8)
P: 257
10章 計算式
*)

// P:257~258
// 例10-1　とある年の月日を列挙するシーケンス式
let daysOfTheYear =
    seq {
        let months =
            [
                "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
            ]
        let daysInMonth = function
            | "Feb" -> 28
            | "Apr" | "Jun" | "Sep" | "Nov" -> 30
            | _ -> 31
        for month in months do
            for day = 1 to daysInMonth month do
                yield (month, day)
    }

// P:258~259
// 普通の回りくどい関数
type Result = Success of float | DivByZero
let divide x y = 
    match y with
    | 0.0 -> DivByZero
    | _ -> Success(x / y)
//// 1/R_e = 1/R_1 + 1/R_2 + 1/R_3
//let totalResistance r1 r2 r3 =
//    let r1Result = divide 1.0 r1
//    match r1Result with
//    | DivByZero -> DivByZero
//    | Success(x) ->
//        let r2Result = divide 1.0 r2
//        match r2Result with
//        | DivByZero -> DivByZero
//        | Success(y) ->
//            let r3Result = divide 1.0 r3
//            match r3Result with
//            | DivByZero -> DivByZero
//            | Success(z) ->
//                let finalResult = divide 1.0 (x + y + z)
//                finalResult
//// P:259
//let totalResistance r1 r2 r3 =
//    let_with_check x = divide 1.0 r1
//    let_with_check y = divide 1.0 r2
//    let_with_check z = divide 1.0 r3
//    return_with_check 1.0 (x + y + z)

//// P:260
//let let_with_check result restOfComputation =
//    match result with
//    | DivByZero -> DivByZero
//    | Success(x) -> restOfComputation x
//let totalResistance r1 r2 r3 =
//    let_with_check (divide 1.0 r1) (fun x ->
//    let_with_check (divide 1.0 r2) (fun y ->
//    let_with_check (divide 1.0 r3) (fun z ->
//    divide 1.0 (x + y + z))))

// P:261
// 例10-4 success/failure の計算式
type DefinedBuilder() =
    member this.Bind ((x:Result), (rest:float -> Result)) =
        match x with
        | Success(x) -> rest x
        | DivByZero -> DivByZero
    member this.Return x = x
let defined = DefinedBuilder()
let totalResistance r1 r2 r3 =
    defined {
        let! x = divide 1.0 r1
        let! y = divide 1.0 r2
        let! z = divide 1.0 r3
        return divide 1.0 (x + y + z)
    }
//実体
let totalResistance2 r1 r2 r3 =
    defined.Bind (
        (divide 1.0 r1),
        (fun x ->
            defined.Bind (
                (divide 1.0 r2),
                (fun y ->
                    defined.Bind (
                        (divide 1.0 r3),
                        (fun z ->
                            defined.Return(
                                divide 1.0 (x + y + z)
    )))))))

let run () =
    1
