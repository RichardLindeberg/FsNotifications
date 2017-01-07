#I "C:\Repos\FsNotifications\packages"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "Aether/lib/net35/Aether.dll"
#r "Chiron/lib/net40/Chiron.dll"
 
#r "System.Runtime.Serialization"

#r "../build/Notifications.Domain.dll"
#r "../packages/Suave/lib/net40/Suave.dll"
#load "json.fs"
#load "web.fs"
open Notifications
let app = SuaveApp.notificationsWeb