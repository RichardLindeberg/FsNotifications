namespace Notifications.Domain
open PersonalNumberParser
type NotificationType = 
    | SooneDue
    | NewInvoice
    | ChangedInvoice

type TokenNotificationId = { Token : string; NotificationTypeId : NotificationType }
type Person = { PersonalNumber : PersonalNumber Option; Tokens : TokenNotificationId list } with
    static member Zero = {PersonalNumber = None; Tokens = []}

type PersonAddRemoveTokenRecord = {  CommandId : System.Guid; PersonalNumber : PersonalNumber; Token : string; NotificationTypeId : NotificationType }

type RemoveTokenDueToDuplcateRecord = {  CommandId : System.Guid; PersonalNumber : PersonalNumber; Token : string; }

type PersonTokenNotification = { CommitId : System.Guid; EventId : System.Guid; PersonalNumber : PersonalNumber; Token : string; NotificationTypeId : NotificationType } with  
    static member FromCommand (cmd : PersonAddRemoveTokenRecord) : PersonTokenNotification = 
        {CommitId = cmd.CommandId; EventId = System.Guid.NewGuid(); PersonalNumber = cmd.PersonalNumber; Token = cmd.Token; NotificationTypeId = cmd.NotificationTypeId}

type PersonEvent = 
    | TokenAdded of PersonTokenNotification
    | TokenRemoved of PersonTokenNotification

type PersonCommand = 
    | AddToken of PersonAddRemoveTokenRecord
    | RemoveToken of PersonAddRemoveTokenRecord
    | RemoveTokenDueToDuplcate of RemoveTokenDueToDuplcateRecord

type PersonCommandFailures = 
    | StreamOpeningException of System.Exception
    | StreamReadingException of System.Exception
    | NoTokenToRemove of string
    | InconsistentCommitIds
    | CommandAlreadyCommited
    | OptimistictConcurrency
    | UnHandledException of System.Exception


type EventVersion = int

type PersonEvents = 
    {
        EventVersion : EventVersion;
        Events : PersonEvent seq
    }

module peopleCommandHandler = 
       
    let canRemoveToken (cmd : PersonAddRemoveTokenRecord) state = 
        let asTokenItem = { TokenNotificationId.Token = cmd.Token; NotificationTypeId = cmd.NotificationTypeId }
        List.exists (fun x -> x = asTokenItem) state.Tokens 

    let execute cmd state  = 
        match cmd with 
            | AddToken v -> 
                match List.exists (fun x -> x =  { TokenNotificationId.Token = v.Token; NotificationTypeId = v.NotificationTypeId }) state.Tokens with 
                    | false ->  [TokenAdded (PersonTokenNotification.FromCommand v)] |> Success
                    | true -> [] |> Success
            | RemoveToken v ->
                match canRemoveToken v state with 
                    | true -> [TokenRemoved (PersonTokenNotification.FromCommand v)] |> Success
                    | false -> sprintf "%s of type %A does not exist on %A" v.Token v.NotificationTypeId  v.PersonalNumber  |> NoTokenToRemove  |> Failed
            | RemoveTokenDueToDuplcate v ->
                let tokenComparer (cmd : RemoveTokenDueToDuplcateRecord) (tokNot : TokenNotificationId) = 
                    cmd.Token = tokNot.Token
                
                let hasToken xs f =
                     List.filter f xs
                
                let commandCreator cmd (x : TokenNotificationId) = 
                    {CommitId = cmd.CommandId; EventId = System.Guid.NewGuid(); PersonalNumber = cmd.PersonalNumber; Token = cmd.Token; NotificationTypeId = x.NotificationTypeId}
                    |> TokenRemoved
                let cmds xs = 
                    List.map (fun x -> commandCreator v x) xs
                
                tokenComparer v 
                |> hasToken state.Tokens
                |> cmds 
                |> Success 

    let apply events =
        let applyOne (s:Person) e = 
            match e with 
                | TokenAdded v -> 
                    let newToken = { TokenNotificationId.Token = v.Token; NotificationTypeId = v.NotificationTypeId }
                    match List.exists (fun x -> x = newToken) s.Tokens with 
                        | false ->  { s with Tokens =  newToken::s.Tokens}
                        | true -> s
                | TokenRemoved v -> 
                    let newToken = { TokenNotificationId.Token = v.Token; NotificationTypeId = v.NotificationTypeId }
                    let otherTokens = List.filter (fun x -> x <> newToken) s.Tokens
                    { s with Tokens = otherTokens}
        Seq.fold applyOne Person.Zero events
        
    
    let executeOn cmd =
        let e' = 
            execute cmd
        apply  
        >> e'   



    



        
