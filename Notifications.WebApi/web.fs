namespace Notifications
open Notifications.Domain

open Suave                 // always open suave
open Suave.Successful      // for OK-result
open Suave.Web             // for config

open Suave.Filters
open Suave.Operators
open Suave.RequestErrors
open Notifications.Commands


type CorrelationId = | CorrelationId of string

type ApiFailures = 
  | BadRequestFailure of string
  | DomainError of Notifications.Domain.PersonCommandFailures

module SuaveApp  = 
    let getXCorrelationId onExist onMissing (ctx : HttpContext) = 
        let p = ctx.request
        match p.header "X-CorrelationId" with
            | Choice1Of2 header -> onExist header
            | Choice2Of2 _ -> onMissing 

    let requireCorrelationId (protectedPart : WebPart) (ctx : HttpContext) =
        let s x = 
            protectedPart ctx
        getXCorrelationId (s) (BAD_REQUEST "No X-CorrelationId supplied" ctx) ctx
    
    let handleApiFailures = function 
        | DomainError _-> NOT_ACCEPTABLE "Dunno"
        | BadRequestFailure y -> BAD_REQUEST y


    let getBodyAsString = 
        let byteToString rawForm = 
            try
            System.Text.Encoding.UTF8.GetString(rawForm) |> Success
            with 
            | :? System.ArgumentNullException as arg -> (BadRequestFailure "No data" |> Failed)
            | :? System.ArgumentException as arg -> (BadRequestFailure "Invalid data" |> Failed)
        byteToString
    
    let parseIt s = 
       match Notifications.Commands.parse s with 
            | Choice1Of2 x -> Success x
            | Choice2Of2 x -> BadRequestFailure x |> Failed

    let executeCommand pstr ctx=
        let pnr = PersonalNumber pstr
        Result.maybe{
                let! correlationId = getXCorrelationId (CorrelationId >> Success) (BadRequestFailure "No acceptHeader" |> Failed) ctx
                let! bodyAsString = getBodyAsString ctx.request.rawForm 
                let! deserializedBody = parseIt bodyAsString
                let cmd = { 
                        Notifications.Domain.PersonAddRemoveTokenRecord.CommandId = System.Guid.NewGuid(); 
                        PersonalNumber = pnr; 
                        Token = deserializedBody.AddTokens.Head; 
                        NotificationTypeId = SooneDue 
                        } 
                let c2 = Notifications.Domain.AddToken cmd 
                return correlationId, bodyAsString, deserializedBody, c2
        }
        |> function 
            | Success x ->
                match x with 
                    | (CorrelationId c, body, cmd, c2) -> OK (sprintf "X-Correlation is %s and your pnr is %A and the body was %s and the deserializedBody is %A, command is %A" c pnr body cmd c2) ctx 
            | Failed x -> (handleApiFailures x) ctx

    let notificationsWeb =
        choose [
                  //  requireCorrelationId <|
                    choose [
                                GET >=> path "/get" >=> OK "Woho you made it" 
                                PATCH >=> pathScan "/%s" executeCommand 
                                NOT_FOUND "No where to be found" 
                            ]
                ]  
