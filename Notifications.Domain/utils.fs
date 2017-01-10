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


    let mod10Check input = 
            let stringToIntList = 
                Seq.map (System.Char.GetNumericValue >> int)
                >> Seq.toList
                          
            let doubleOrFactor x = 
                if x * 2 > 9 then 
                    x * 2 - 9 
                else 
                    x * 2

            let modByXIsY x y z =
                z % x = y  
                
            let multiply x y = 
                x * y 

            let printIt a =
                printfn "It is %A" a
                a 
            
            let calcOnEven calcFn  index x =
                match index % 2 = 0 with 
                    | true ->  x 
                    | false -> calcFn x
            
            let luhnMapping xs =
                let calculator = calcOnEven doubleOrFactor 
                List.mapi calculator xs
            
            input 
            |> stringToIntList
            // calculate from right to left
            |> List.rev
            |> luhnMapping
            |> List.sum
            |> multiply 9
            |> modByXIsY 10 0
            |> function
                | true -> Success input
                | false -> Failed "Incorrect checksum"


            // (List.collect intToEntalsLista (fst data |> List.mapi doubleIfEvenIndex))
            // |> List.sum
            // |> (string >> stringToIntList)
            // |> last
            // |> checkSiffra
            // |> sameNumber (snd data)
            // |> function
            //     | Some x -> Success input
            //     | None -> Failed "Incorrect checksum"

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
