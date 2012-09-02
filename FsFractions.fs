namespace FsCommons

/// 分数ライブラリ
module Fractions =
    // Determine the highest common factor
    // two positive integers, a helper for reducing factions.
    let rec hcf a b =
        if a = 0I then b
        elif a < b then hcf a (b-a)
        else hcf (a-b) b

    /// 分数のレコード
    type Fraction =
        {
            /// 分子
            n : bigint
            /// 分母
            d : bigint
        }

        // Produce a string representation.
        // denominator is "1", do not display it.
        override x.ToString() =
            if (x.d = 1I) then sprintf "%A" x.n
            else sprintf "%A/%A" x.n x.d

        /// 約分？
        static member Reduce a b =
            let c = hcf a b
            {
                n = a / c
                d = b / c
            }
        ///仮分数化
        static member Improper (f1:Fraction) i = { n = f1.d * i; d = f1.d }
        /// 分数の足し算引き算
        static member AddSub op f (f1:Fraction, f2:Fraction) =
            let (a,b) = f1.n * f2.d, f2.n * f1.d
            f a b
            let nTemp = op a b
            let dTemp = f1.d * f2.d
            Fraction.Reduce nTemp dTemp

        /// 足し算
        static member (+) (f1:Fraction, f2:Fraction) = Fraction.AddSub (+) (fun _ _ -> ()) (f1,f2)
        /// 足し算
        static member (+) (f1:Fraction, i) = f1 + (Fraction.Improper f1 i)
        /// 足し算
        static member (+) (i, f2:Fraction) = f2 + (Fraction.Improper f2 i)
        /// 引き算
        static member (-) (f1:Fraction, f2:Fraction) =
            let f a b =
                if a < b then failwith "This operation results in a negative number, which is not supported."
            Fraction.AddSub (-) f (f1,f2)
        /// 引き算
        static member (-) (f1:Fraction, i) = f1 - (Fraction.Improper f1 i)
        /// 引き算
        static member (-) (i, f2:Fraction) = f2 - (Fraction.Improper f2 i)
        /// かけ算
        static member (*) (f1:Fraction, f2:Fraction) =
            let nTemp = f1.n * f2.n
            let dTemp = f1.d * f2.d
            Fraction.Reduce nTemp dTemp
        /// かけ算
        static member (*) (f1:Fraction, i) = f1 * (Fraction.Improper f1 i)
        /// かけ算
        static member (*) (i, f2:Fraction) = f2 * (Fraction.Improper f2 i)
        /// 割り算
        static member (/) (f1:Fraction, f2:Fraction) = f1 * { n = f2.d; d = f2.n }
        /// 割り算
        static member (/) (f1:Fraction, i) = f1 / (Fraction.Improper f1 i)
        /// 割り算
        static member (/) (i, f2:Fraction) = (Fraction.Improper f2 i) / f2

