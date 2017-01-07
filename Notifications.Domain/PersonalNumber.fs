namespace Notifications.Domain
module PersonalNumberParser =
    open Utils
    type Age = 
        | Over100Years
        | Under100Years
        | Unknown
    
    let pnrStandardFormat birthPart birthNumber controlNumber= 
        sprintf "%s-%s%i" birthPart birthNumber controlNumber

    type PersonalNumber = {BirthPart : string; BirthNumber : string; ControlNumber : int; Age : Age} with
        member this.ToString = pnrStandardFormat this.BirthPart this.BirthNumber this.ControlNumber

    let create bp bn c a = 
        {PersonalNumber.BirthPart = bp; Age = a; BirthNumber = bn; ControlNumber = c}
    
    let verifyPersonalNumber (p : PersonalNumber) =
        let woC = 
            sprintf "%s%s" p.BirthPart p.BirthNumber
        let stringToIntList s = 
            [for c in s -> c.ToString() |> int]
   
        let intToEntalsLista i = 
            i |> string |> stringToIntList

        let rec last = function
            | [x] -> x
            | hd :: tl -> last tl
            | _ -> failwith "Empty list."
        
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
        
        (List.collect intToEntalsLista (woC |> stringToIntList |> List.mapi doubleIfEvenIndex))
        |> List.sum
        |> intToEntalsLista
        |> last
        |> checkSiffra
        |> sameNumber p.ControlNumber
        |> function
            | Some x -> Some p
            | None -> None

    let pnas bpStr nrStr cStr age = 
        maybe{
                    let! bp = strToSomeInt bpStr 
                    let! nr = strToSomeInt nrStr
                    let! c = strToSomeInt cStr
                    let! pnr = create bpStr nrStr c age |> verifyPersonalNumber 
                    return pnr
        }
        
    let parse s = 
        match String.length(s) with 
            | 12 -> 
                let p = pnas s.[2..7] s.[8..10] (s.[11].ToString())
                match s.[0..1] |> strToSomeInt with 
                    | Some i ->  
                         if 20 - i > 0 
                            then p Over100Years 
                            else p Under100Years
                    | None -> None
               
            | 11 -> 
                let sep = s.[6]  
                let p = pnas s.[0..5] s.[7..9] (s.[10].ToString())
                match sep with 
                        | '-' -> p Over100Years 
                        | '+' -> p Under100Years  
                        | _ -> None
            | 10 -> 
                let p = pnas s.[0..5] s.[6..8] (s.[9].ToString())
                p Unknown               
            | _ -> None

     
