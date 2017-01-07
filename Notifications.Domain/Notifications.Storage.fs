module Notifications.Storage

open Notifications.Domain
open NEventStore

module Advanced = 
    let openStream (store : IStoreEvents) (streamId : string) =
        try
            store.OpenStream(streamId, 0) |> Success
        with 
            | e -> StreamOpeningException e |> Failed

    let toEventMessages evt = 
        let em = new EventMessage()
        em.Body <- evt
        em

    let persistEvents (stream : IEventStream) (xs : PersonEvent list) (cid : System.Guid) = 
            try
                List.map toEventMessages xs
                |> List.iter stream.Add 
            
                stream.CommitChanges cid
                Success ""
            with
                | :? NEventStore.DuplicateCommitException as ex ->  Failed CommandAlreadyCommited
                | :? NEventStore.ConcurrencyException as ex -> Failed OptimistictConcurrency
                | ex -> UnHandledException ex |> Failed

    let addToStream (stream : IEventStream) (evts : PersonEvent list) = 
        let getCommitIdFromPersonEvent = function
            | TokenAdded x -> Success x.CommitId
            | TokenRemoved x -> Success x.CommitId

        let sameCommitIdInList xs commitId =
            let all f xs = 
                match List.filter f xs |> Seq.length with 
                    | 0 -> true
                    | _ -> false
            let isNotEqual m1 m2 = 
                match m1 with 
                    | Success x -> x <> m2
                    | Failed f -> true
                    
            match all (fun x -> isNotEqual (getCommitIdFromPersonEvent x) commitId) xs with 
                | true -> Success commitId
                | false -> Failed InconsistentCommitIds
        
        match evts with 
            | head :: tail ->  
                        Result.maybe {
                            let! streamId = getCommitIdFromPersonEvent head
                            let! commitId = sameCommitIdInList evts streamId
                            let! res = persistEvents stream evts commitId
                            return res 
                        }
            | [] ->  Success "" // No events is still successful
        
    let getData (stream : IEventStream) = 
        let allAre (a:obj) = 
            match a with 
                    | :? PersonEvent as sds -> true 
                    | _ -> false
        try
            stream.CommittedEvents 
            |> Seq.filter (fun x -> match x.Body with 
                                    | :? PersonEvent as sds -> true 
                                    | _ -> false) 
            |> Seq.map (fun x -> x.Body :?> PersonEvent) 
            |> Seq.toList
            |> Success
        with 
            | e -> StreamReadingException e |> Failed

open Advanced
let commandHandler store cmd = 
    let cmdKey = 
      function 
            | AddToken x -> x.PersonalNumber
            | RemoveToken x -> x.PersonalNumber
            | RemoveTokenDueToDuplcate x -> x.PersonalNumber
   
    let pnrToStreamId =
        function 
            | PersonalNumber x -> sprintf "Person-%s" x

    Result.maybe{
       let streamId =  cmd |> cmdKey |> pnrToStreamId
       use! stream =  openStream store streamId
       let! oldEvents =  getData stream
       let! newEvents = peopleCommandHandler.executeOn cmd oldEvents
       let! res = addToStream stream newEvents
       return res
    }
    

let streamToText store streamId = 
    Result.maybe{
        use! stream = openStream store streamId
        let! events = getData stream
        return Seq.map (fun x -> sprintf "%A" x)
    }

let storeToText (store : IStoreEvents) =
    let cmts = store.Advanced.GetFrom ""
    let printCommit (c : ICommit) = 
        Seq.map (fun (x : EventMessage) -> sprintf "Event: %A" x.Body) c.Events
    Seq.collect printCommit cmts