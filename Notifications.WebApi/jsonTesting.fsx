#I "C:\Repos\FsNotifications\packages"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "Aether/lib/net35/Aether.dll"
#r "Chiron/lib/net40/Chiron.dll"
 
#r "System.Runtime.Serialization"
open Chiron
open Chiron.Operators
open Chiron.Formatting

type ChangeToken = 
    {NotificationId : string; AddTokens : string list ; RemoveTokens : string list }
    static member ToJson (x: ChangeToken) = 
        Json.write "notificationId" x.NotificationId
        *> Json.write "addTokens" x.AddTokens
        *> Json.write "removeTokens" x.RemoveTokens
        

    static member FromJson (_: ChangeToken) = 
        json {
            let! n = Json.read "notificationId"
            let! a = Json.read "addTokens"
            let! r = Json.read "removeTokens"
            return {NotificationId = n; AddTokens = a; RemoveTokens = r}
        }
let s1 (c : ChangeToken) = 
    c |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact

type JsonErrors = 
    | ParseError of string
    | DeserializeError of string
    | SerializeError of string

let tryWrap fe fs m = 
    try
      fs m |> Choice1Of2
    with 
      | e -> fe e.Message |> Choice2Of2

let bind1Of2 f m = 
    match m with 
        | Choice1Of2 v -> f v
        | Choice2Of2 v -> Choice2Of2 v


let parse s : Choice<ChangeToken, string> = 
    let inner = Json.tryParse >> bind1Of2 Json.tryDeserialize
    s |> inner

let test () = 
    //  let c1 = {NotificationId = "NOT1"; AddTokens =  ["tok1"]; RemoveTokens = []}
    //  printfn "c1 = %A" c1
    //  let s = s1 c1
    //  printfn "s = %s" s
    //  let c2 = s |> parse
    //  printfn "c2 = %A" c2 
     let s2 = """
        {"addTokens":["tok1"],"notificationId":"NOT1","removeTokens":""}
        """
     printfn "s = %s" s2
     let c3 = s2 |> parse
     c3


test ()
