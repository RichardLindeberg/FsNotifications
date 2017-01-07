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
            
            let stringToIntList s = 
                [for c in s -> c.ToString() |> int]
            
            let strLista = stringToIntList input
            
            let woC  = 
                let l = -1 + List.length strLista 
                List.take l strLista

            let rec last = function
                | [x] -> x
                | hd :: tl -> last tl
                | _ -> failwith "Empty list."

            let data = (woC, last strLista)    

            let intToEntalsLista  = 
                string >> stringToIntList

        
            
            let doubleIfEvenIndex index item =
                match index % 2 with 
                            | 0 -> item * 2 
                            | _ -> item  
                            
            let checkSiffra i =
                let c = 10 - i 
                match c with 
                    | 10 -> 0
                    | _ -> c
            
            let sameNumber i1 i2 = 
                match i1 = i2 with 
                    | true -> Some i1
                    | false -> None
            
            (List.collect intToEntalsLista (woC |> List.mapi doubleIfEvenIndex))
            |> List.sum
            |> intToEntalsLista
            |> last
            |> checkSiffra
            |> sameNumber (last strLista)
            |> function
                | Some x -> Success input
                | None -> Failed "Incorrect checksum"

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
