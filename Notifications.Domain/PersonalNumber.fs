namespace Notifications.Domain
module PersonalNumberParser =
    open Utils
    open System
    type CenturyCertainty = 
        | Certain
        | Uncertain    
    let pnrStandardFormat birthPart birthNumber controlNumber= 
        sprintf "%s-%s%i" birthPart birthNumber controlNumber

    type PersonalNumberRecord = {BirthPart : string; BirthNumber : string; ControlNumber : int; CenturyCertainty : CenturyCertainty; DateOfBirth : DateTime} with
         override  this.ToString () = pnrStandardFormat this.BirthPart this.BirthNumber this.ControlNumber

    type OrganizationalNumber = {Identifier: string; ControlNumber : int}

    type SwedishIdentifier = 
        | PersonalNumber of PersonalNumberRecord
        | CordinationNumber of PersonalNumberRecord
        | OrganizationalNumber of OrganizationalNumber

    let create bp bn c a db= 
        {PersonalNumberRecord.BirthPart = bp; CenturyCertainty = a; BirthNumber = bn; ControlNumber = c; DateOfBirth = db}
    
    let verifyPersonalNumber (p : PersonalNumberRecord) =
        let str = 
            sprintf "%s%s%i" p.BirthPart p.BirthNumber p.ControlNumber
        
        match Utils.calculateLuhnCheckSum str with 
            | Success x -> Success p
            | Failed f -> Failed f
    

    let pnas bpStr nrStr cStr age dateOfBirth= 
                Result.maybe{
                            let! bp = strToResultInt bpStr 
                            let! nr = strToResultInt nrStr
                            let! c = strToResultInt cStr
                            let! pnr = create bpStr nrStr c age dateOfBirth |> verifyPersonalNumber 
                            return pnr
                }
    type PnrParser = PnrParser of (string -> Result<SwedishIdentifier, string>)

    let subStr f t (s : string) = 
        s.[f..t]

    let actOn12 f s = 
        if (String.length(s)) = 12 then 
            f 
        else
            Failed "Not applicable"

    let personNummerToDateString (s : string) =  
         sprintf "%s-%s-%s" s.[0..3] s.[4..5] s.[6..7]


    let cordinationnumberToDateString (bpFn : string -> string) = 
        let dateParser (s : string) = 
            Result.maybe{
                let bp = bpFn s
                let datePart = bp.[6..7]
                let! dateAsInt = strToResultInt datePart 
                let date60Less = dateAsInt - 60
                let dateStr = date60Less |> string 
                let date =  sprintf "%s-%s-%s" bp.[0..3] bp.[4..5] dateStr
                return sprintf "%s-%s-%s" bp.[0..3] bp.[4..5] dateStr
            } 
        dateParser

    // let cordinationNummerToDateString (s : string) =
    //      Result.maybe{
    //         let datePart = s.[6..7]
    //         let! dateAsInt = strToResultInt datePart 
    //         let date60Less = dateAsInt - 60
    //         let dateStr = date60Less |> string 
    //         return sprintf "%s-%s-%s" s.[0..3] s.[4..5] dateStr
    //      }

         

    type PP = 
        {
            ActOn : (string -> bool)
            BirthPartFn : (string -> string)
            BirthNrFn :  (string -> string)
            ControllNrFn : (string -> string)
            DateToStrParser : (string -> Result<DateTime, string>)
            CenturyCertainty : (string -> Result<CenturyCertainty, string>)
            RTParser : (PersonalNumberRecord -> SwedishIdentifier)
        }

    let ppRunner (p : PP) s = 
         if (p.ActOn s) then
            Result.maybe{
                            let bpStr = p.BirthPartFn s
                            let! bp = bpStr |> strToResultInt
                            let nrStr = p.BirthNrFn s
                            let! nr = nrStr |> strToResultInt
                            let cStr = p.ControllNrFn s
                            let! c = cStr |> strToResultInt 
                            let! dateOfBirth = p.DateToStrParser s
                            let! age = p.CenturyCertainty s
                            let! pnr = create bpStr nrStr c age dateOfBirth |> verifyPersonalNumber 
                            return p.RTParser pnr
            }
         else
            Failed ""
    
    let thirteenDigitPersonalNumberParser = 
        {
                PP.BirthPartFn = subStr 2 7; 
                PP.BirthNrFn = subStr 9 11;
                PP.ControllNrFn = subStr 12 12;
                PP.CenturyCertainty = (fun x -> Success Certain);
                PP.DateToStrParser = personNummerToDateString >> strToResultDateTime
                PP.RTParser = PersonalNumber
                PP.ActOn = (fun x -> String.length(x) = 13)
        }
    let thirteenDigitCordinationNumberParser = 
        { thirteenDigitPersonalNumberParser with
             DateToStrParser = thirteenDigitPersonalNumberParser.BirthPartFn |> cordinationnumberToDateString  >> Result.bind strToResultDateTime;
             RTParser = CordinationNumber
        }

        
    let twelveDigitPersonalNumberParser = 
        {
                
                PP.BirthPartFn = subStr 2 7; 
                PP.BirthNrFn = subStr 8 10;
                PP.ControllNrFn = subStr 11 11;
                PP.CenturyCertainty = (fun x -> Success Certain);
                PP.DateToStrParser = personNummerToDateString >> strToResultDateTime
                PP.RTParser = PersonalNumber
                PP.ActOn = (fun x -> String.length(x) = 12)
        }

    let twelveDigitCordinationNumberParser = 
        { twelveDigitPersonalNumberParser with
             DateToStrParser = twelveDigitPersonalNumberParser.BirthPartFn |> cordinationnumberToDateString  >> Result.bind strToResultDateTime;
             RTParser = CordinationNumber
        }


    let elevenDigitPersonalNumberParser =
        let dateParser (s : string) = 
            let sep = s.[6]
            match sep with 
                        | '+' ->  
                            GetGuessedAgeFrom6DigitString (DateTime.Now.Year - 2000 + 1) (subStr 0 5 s)
                        | '-' ->
                            GetGuessedAgeFrom6DigitString 0 (subStr 0 5 s)
                        | _ -> Failed ""
        {
                PP.BirthPartFn = subStr 0 5; 
                PP.BirthNrFn = subStr 7 9;
                PP.ControllNrFn = subStr 10 10;
                PP.CenturyCertainty = (fun x -> Success Certain)
                PP.DateToStrParser = dateParser;
                PP.RTParser = PersonalNumber
                PP.ActOn = (fun x -> String.length(x) = 11)
        }

    let tenDigitPersonalNumberParser =
       {
                PP.BirthPartFn = subStr 0 5; 
                PP.BirthNrFn = subStr 6 8;
                PP.ControllNrFn = subStr 9 9;
                PP.CenturyCertainty = (fun x -> Success Uncertain);
                PP.DateToStrParser = (fun x -> GetGuessedAgeFrom6DigitString 0 (subStr 0 5 x));
                PP.RTParser = PersonalNumber
                PP.ActOn = (fun x -> String.length(x) = 10)
        }

    let rec test (xs : PP list) (s : string) = 
        match xs with 
           | [h] -> ppRunner h s
           | h::t -> match ppRunner h s with 
                            | Success x -> Success x
                            | Failed _ -> test t s 
           | [] -> Failed "No parsers"

    
    let parse s = 
        let parsers = 
            [
                thirteenDigitPersonalNumberParser;
                twelveDigitPersonalNumberParser; 
                elevenDigitPersonalNumberParser; 
                tenDigitPersonalNumberParser; 
                thirteenDigitCordinationNumberParser;
                twelveDigitCordinationNumberParser;
            ]
        test parsers s
        // match String.length(s) with 
        //     | 12 -> 
        //          ppRunner twelveDigitPersonalNumberParser s
                 
        //     | 11 -> 
        //         ppRunner elevenDigitPersonalNumberParser s
        //     | 10 -> 
        //         ppRunner tenDigitPersonalNumberParser s
        //     | _ -> Failed "Invalid input"

     

