namespace Notifications.Domain

type Result<'success, 'failure> = 
    | Success of 'success
    | Failed of 'failure

module Utils = 
    type MaybeBuilder() =
        member this.Bind(x, f) = 
            match x with
            | None -> None
            | Some a -> f a
        member this.Return(x) = 
            Some x

    let maybe = new MaybeBuilder()

    let strToSomeInt str =
        match System.Int32.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None
    
    let strToResultInt str =
        match System.Int32.TryParse(str) with
        | (true,int) -> Success int
        | _ -> Failed <| sprintf "Not an int %s" str


module Result =
    let bind f v = 
        match v with 
            | Success x -> f x
            | Failed x -> Failed x

    let apply m = 
        Success m

    let (>>=) f m = 
        f m  

    type ResultBuilder() = 
        member this.Bind (x, f) =
            bind f x

        member this.Return (x) = 
            apply x

        member this.ReturnFrom (x) = 
            printfn "returnFrom %A" x
            x

        member this.TryFinally(body, compensation) =
            try 
                printfn "TryFinally Body"
                this.ReturnFrom(body())
            finally 
                printfn "TryFinally compensation"
                compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                    match disposable with 
                        | null -> () 
                        | disp -> disp.Dispose())

    let maybe = ResultBuilder()
