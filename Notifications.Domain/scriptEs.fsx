#r @"C:\Repos\FsNotifications\Notifications.Domain\bin\Debug\NEventStore.dll"
#load "Notifications.Domain.fs"
#load "Notifications.Storage.fs"
#load "Notifications.ReadModels.fs"

open Notifications.Domain
open NEventStore
open Notifications.Storage
open Notifications.ReadModels

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
    { PersonAddRemoveTokenRecord.CommandId = cmdId; PersonalNumber = PersonalNumber <| pnr ;Token = token; NotificationTypeId = notId }
    |> AddToken

let cmds = [
    getAddCommand (System.Guid.NewGuid(), SooneDue, "1", "ABCDE");
    getAddCommand (System.Guid.NewGuid(), NewInvoice, "2", "ABCD");
    getAddCommand (System.Guid.NewGuid(), SooneDue, "3", "ABCDE");
    getAddCommand (System.Guid.NewGuid(), SooneDue, "4", "ABCDE");
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