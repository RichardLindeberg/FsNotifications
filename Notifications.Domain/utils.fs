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

    let strToResultDateTime str =
        match System.DateTime.TryParse(str) with
        | (true,int) -> Success int
        | _ -> Failed <| sprintf "Not an DateTime %s" str

    let calculateLuhnCheckSum (input : string) = 
            let doubleOrFactor x = 
                if x * 2 > 9 then 
                    x * 2 - 9 
                else 
                    x * 2

            let calcOnEven calcFn  index x =
                match index % 2 = 0 with 
                    | true ->  x 
                    | false -> calcFn x
            
            let luhnMapping =
                let calculator = calcOnEven doubleOrFactor 
                List.mapi calculator
            
            let bind f m  = 
                match m with 
                    | (x, y) -> x , f y

            let toTuple m = 
                m , m 
            
            let (>>=) m f = bind f m

            toTuple input 
            >>= (Seq.map (System.Char.GetNumericValue >> int))
            >>= Seq.toList
            // // calculate from right to left
            >>= List.rev
            >>= luhnMapping
            >>= List.sum
            >>= (fun x -> 9 * x)
            >>= (fun x -> x % 10 = 0)
            |> function
                 | (x ,true) -> Success x
                 | (x, false) -> sprintf "Incorrect checksum, %A" x |> Failed
            


    let GetGuessedAgeFrom6DigitString ageThreshold (s : string) =
        let twoDigitYear (s : string) =
            if (s.Length <> 6) then 
                failwith "Incorrect length of string" 
            else
                s.[0..1] |> int

        let guessedYearOfBith t =
            // Yes this code will break after new years eve 2100
            if (t + ageThreshold + 2000) > (System.DateTime.Now.Year) then 
                1900 + t
            else
                2000 + t

        let guessedAge x = 
            System.DateTime.Now.Year - x    

        let gb = twoDigitYear >> guessedYearOfBith 

        let guessedAge = gb s 
        let guessBirthDate = sprintf "%i-%s-%s" guessedAge s.[2..3] s.[4..5]
        strToResultDateTime guessBirthDate 



    
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
