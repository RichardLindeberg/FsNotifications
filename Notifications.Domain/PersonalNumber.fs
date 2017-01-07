namespace Notifications.Domain
module PersonalNumberParser =
    open Utils
    open System
    type AgeCertainty = 
        | Certain
        | Uncertain    
    let pnrStandardFormat birthPart birthNumber controlNumber= 
        sprintf "%s-%s%i" birthPart birthNumber controlNumber

    type PersonalNumber = {BirthPart : string; BirthNumber : string; ControlNumber : int; AgeCertainty : AgeCertainty; DateOfBirth : DateTime} with
         override  this.ToString () = pnrStandardFormat this.BirthPart this.BirthNumber this.ControlNumber
    
    let create bp bn c a db= 
        {PersonalNumber.BirthPart = bp; AgeCertainty = a; BirthNumber = bn; ControlNumber = c; DateOfBirth = db}
    
    let verifyPersonalNumber (p : PersonalNumber) =
        let str = 
            sprintf "%s%s%i" p.BirthPart p.BirthNumber p.ControlNumber
        
        match Utils.mod10Check str with 
            | Success x -> Success p
            | Failed f -> Failed f

    let parse s = 
        let pnas bpStr nrStr cStr age dateOfBirth= 
                Result.maybe{
                            let! bp = strToResultInt bpStr 
                            let! nr = strToResultInt nrStr
                            let! c = strToResultInt cStr
                            let! pnr = create bpStr nrStr c age dateOfBirth |> verifyPersonalNumber 
                            return pnr
                }
                
        match String.length(s) with 
            | 12 -> 
                let p = pnas s.[2..7] s.[8..10] (s.[11].ToString()) Certain
                let dtStr = sprintf "%s-%s-%s" s.[0..3] s.[4..5] s.[6..7]
                match strToResultDateTime dtStr with 
                    | Success x -> p x
                    | Failed f -> Failed <| sprintf "Invalid input: %s (%s)" f s.[0..7] 
                
            | 11 -> 
                let sep = s.[6]  
                let p = pnas s.[0..5] s.[7..9] (s.[10].ToString()) Certain
                match sep with 
                        | '+' ->
                             let th = DateTime.Now.Year - 2000 + 1
                             let db = GetGuessedAgeFrom6DigitString th s.[0..5]
                             match db with 
                                | Success x -> p x
                                | Failed f -> Failed "Invalid input" 
                              
                        | '-' -> 
                             let th = 0
                             let db = GetGuessedAgeFrom6DigitString th s.[0..5]
                             match db with 
                                | Success x -> p x
                                | Failed f -> Failed "Invalid input"
                        | _ -> Failed "Invalid input"
            | 10 -> 
                let p = pnas s.[0..5] s.[6..8] (s.[9].ToString()) Uncertain 
                let th = 15
                let db = GetGuessedAgeFrom6DigitString th s.[0..5]
                match db with 
                    | Success x -> p x
                    | Failed f -> Failed "Invalid input" 
                
            | _ -> Failed "Invalid input"

     
