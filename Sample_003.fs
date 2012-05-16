module Sample_003

open System.Diagnostics
open System.Reflection

// 計算式ビルダの実験
type ComputationExpression01 () =
    let trace() =
        (new StackTrace()).GetFrame(1).GetMethod().Name
        |> (printfn "[%s]")
    member this.Bind(v,f) = trace(); f v
    member this.Delay f = trace(); f()
    member this.Return v = trace(); v
    member this.ReturnFrom v = trace(); v
    member this.Run v = trace(); v
    member this.Combine (a,b) = trace(); a@b
    member this.For (s,f) =
        trace()
        s |> Seq.map(fun x -> f x)
    member this.While (f, v) = trace(); v
    member this.Yield v = trace(); v
    member this.YieldFrom v = trace(); v
    member this.Zero () = trace(); -1
let comp = new ComputationExpression01()

let run () =
    let rec comp' =
        comp {
            let b = 2
            let! a = 1
            
            yield [2]
            yield [3]
            return [4]
        }
    comp'
