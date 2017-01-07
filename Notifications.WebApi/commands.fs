namespace Notifications
open Chiron
open Chiron.Operators
open Chiron.Formatting

module Commands = 
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
