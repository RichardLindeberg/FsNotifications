#r @"C:\Repos\FsNotifications\Notifications.Domain\bin\Debug\NEventStore.dll"
#load "utils.fs"
#load "PersonalNumber.fs"
#load "Notifications.Domain.fs"
#load "Notifications.Storage.fs"
#load "Notifications.ReadModels.fs"

open Notifications.Domain
open NEventStore
open Notifications.Storage
open Notifications.ReadModels
open PersonalNumberParser

let getstore () = 
   Wireup.Init()
    .UsingInMemoryPersistence()
    .InitializeStorageEngine()
    .UsingJsonSerialization()
    .Build()

let store = 
    getstore ()

DuplicateTokenRemover.startReadModell store Begining


let cmdId = 
    System.Guid.NewGuid()

let getAddCommand (cmdId, notId,  pnr, token)  = 
    let pMaybe = PersonalNumberParser.parse pnr
    match pMaybe with 
        | Success p ->  
            { PersonAddRemoveTokenRecord.CommandId = cmdId; PersonalNumber = p ;Token = token; NotificationTypeId = notId }
            |> AddToken
        | Failed f -> failwith f

let cmds = [
    getAddCommand (System.Guid.NewGuid(), SooneDue, "8004120351", "ABCDE");
    getAddCommand (System.Guid.NewGuid(), NewInvoice, "8004120351", "ABCD");
    getAddCommand (System.Guid.NewGuid(), SooneDue, "8004120351", "ABCDE");
    getAddCommand (System.Guid.NewGuid(), SooneDue, "8004120351", "ABCDE");
    ]

let ch = 
    commandHandler store
let resultToString = 
    function 
        | Success s -> sprintf "Executed successfully %A" s
        | Failed f -> sprintf "Failed to execute %A" f
let executeCommandsAndPrintResults xs () = 
    List.iter (ch >> resultToString >> (printfn "%s")) xs

executeCommandsAndPrintResults cmds ()