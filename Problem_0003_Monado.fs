module Problem_0003_Monado

(*
【問題】
13195 の素因数は 5、7、13、29 である。
600851475143 の素因数のうち最大のものを求めよ。
A. 6857
*)

type PrimeEnumeratorBuilder() =
    let rec check (p,n) =
        if n < p then None
        elif n%p=0I then Some(p,(p,(n/p)))
        else check(p+1I,n)
    member this.Bind(v,f)= f <| Seq.unfold check (2I,v)
    member this.Return v = v |> Seq.toList
let pe = PrimeEnumeratorBuilder()
type PrimeEnumeratorBuilder2() =
    member this.For(s,f) =
        let m = s.GetType().GetField("m").GetValue(s).ToString() |> (fun x -> bigint.Parse(x))
        let rec check (p,n) =
            if n < p then None
            elif n % p = 0I then Some(p, (p, n/p))
            else check(p+1I, n)
        Seq.unfold check (2I, m)
        |> Seq.map(fun x -> f x)
    member this.Yield v = v
    member this.Run v = v |> Seq.max
let pe2 = PrimeEnumeratorBuilder2()

let run() =
//    pe {
//        let! a = 600851475143I
//        return a
//    }
    pe2 { for x = 0I to 600851475143I do yield x }
