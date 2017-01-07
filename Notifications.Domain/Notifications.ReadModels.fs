namespace Notifications.ReadModels
open Notifications.Domain

open NEventStore
open Notifications.Storage
open PersonalNumberParser

type StartSubscriptionFrom = 
    | Begining
    | CheckPointToken of string

module ReadModelHelpers = 
    let mapFindOrDefault key def map  = 
            Map.tryFind key map 
            |> function
                | Some b -> b
                | None -> def 

    let addIfMissing x xs = 
        match List.contains x xs with 
            | true -> xs
            | false -> x :: xs

    let removeIfPresent x xs = 
        List.filter (fun y -> y <> x) xs 

    let mappAdd m k x = 
        Map.add k x m 


module DuplicateTokenRemover =
    open ReadModelHelpers
    let emptyReadModel : Map<string, PersonalNumber list>= 
        Map.empty<string, PersonalNumber list>
    
    let readModelGenerator m1  = 
        let mapAction pnr token  f = 
            mapFindOrDefault token [] m1
            |> f pnr
            |> mappAdd m1 token 
        
        function
            | TokenAdded v -> mapAction v.PersonalNumber v.Token addIfMissing
            | TokenRemoved v -> mapAction v.PersonalNumber v.Token removeIfPresent

    let removeDuplicateTokensFromOther (store : IStoreEvents) (rm : Map<string, PersonalNumber list>) (evt : PersonEvent)  = 
        let onAddEvent (pnr : PersonalNumber, token : string) = 
            let executeCommandAndPrintResult (store : IStoreEvents) (c : PersonCommand) = 
                    let g = commandHandler store c 
                    match g with 
                        | Success _ -> printfn "Removed token!"
                        | Failed f -> printfn "Remove duplicate failed %A" f

            let getNewRemoveCommand pnr = 
                { RemoveTokenDueToDuplcateRecord.CommandId = System.Guid.NewGuid(); PersonalNumber = pnr ;Token = token }
                |> RemoveTokenDueToDuplcate

            mapFindOrDefault token [] rm
            |> removeIfPresent pnr
            |> List.map getNewRemoveCommand 
            |> List.iter (executeCommandAndPrintResult store)  

        
        match evt with
            | TokenAdded v -> onAddEvent (v.PersonalNumber, v.Token) 
            | TokenRemoved _ ->    printfn "Ignoring remove command"
        
    let personReadModelAgent store = MailboxProcessor.Start(fun inbox-> 

        // the message processing function
        let rec messageLoop (store : IStoreEvents) oldState = async{
            
            // read a message
            let! msg = inbox.Receive()
            
            let newState = readModelGenerator oldState msg
            // process a message
            printfn "message is: %A"  msg
            printfn "state is %A" newState
            printfn "Should go and remove duplicates"
            let something = removeDuplicateTokensFromOther store newState msg

            printfn "Duplicates should be removed"
            // loop to top
            return! messageLoop store newState  
            }

        // start the loop 
        messageLoop store emptyReadModel
        )

    let commitPoster (pa : MailboxProcessor<PersonEvent>) (x : NEventStore.ICommit) = 
        let postEvent (e : obj) = 
            match e with 
                | :? PersonEvent as pe -> pa.Post pe
                | x -> printfn "was something else %A" x
        Seq.iter (fun (e : NEventStore.EventMessage) -> postEvent e.Body )  x.Events
    
    
    let startReadModell (store : IStoreEvents) (startFrom : StartSubscriptionFrom)  = 
        let cpt = 
            match startFrom with 
                | Begining -> ""
                | CheckPointToken s -> s
        let pollingClient = new NEventStore.Client.PollingClient(store.Advanced, 200)
        let pollingClientObserver = pollingClient.ObserveFrom(cpt)

        let something = pollingClientObserver.Start()
        personReadModelAgent store 
        |> commitPoster 
        |> pollingClientObserver.Add 

        pollingClientObserver.PollNow ()

