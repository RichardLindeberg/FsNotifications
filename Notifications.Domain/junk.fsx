#load "utils.fs"
#load "PersonalNumber.fs"
open Notifications.Domain



printfn "guessed: %A " <| PersonalNumberParser.parse "19800472-0351"
//printfn "guessed: %A " <| PersonalNumberParser.parse "198004120351"
//printfn "guessed: %A " <| PersonalNumberParser.parse "120406-2366"
//printfn "guessed: %A " <| PersonalNumberParser.parse "1204062366"
                         
