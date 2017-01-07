#I "C:\Repos\FsNotifications"
#r "packages/NodaTime/lib/net35-Client/NodaTime.dll"
#r "packages/Hopac/lib/net45/Hopac.Core.dll"
#r "packages/Hopac/lib/net45/Hopac.dll"
#r "packages/Logary/lib/net40/Logary.dll"
#r "packages/Logary.Adapters.Suave/lib/net40/Logary.Adapters.Suave.dll"

open System
open Hopac.Core
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets

open Logary.Adapters

let internal logger = 
    Logary.Adapters



let work () =
  logger.info (eventX "Started work")
  48

let logary = 
    withLogary "MyTest" (
        withTargets [
            Console.create Console.empty "console"
            Debugger.create Debugger.empty "debugger" 
        ] >> withRules [
            Rule.createForTarget "console"
            Rule.createForTarget "debugger"
        ]
    )
let logger = Logging.getCurrentLogger ()
Message.event Info "User logged in"
|> Message.setField "user" "user.name"
|> Message.setFieldFromObject "picture" "user.bitmap"
|> Logger.logSimple logger