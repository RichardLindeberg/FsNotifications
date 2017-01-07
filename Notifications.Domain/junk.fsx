#load "utils.fs"
#load "PersonalNumber.fs"
open Notifications.Domain



printfn "guessed: %A " <| PersonalNumberParser.parse "198004120351"
printfn "guessed: %A " <| PersonalNumberParser.parse "120406-2366"
                        
