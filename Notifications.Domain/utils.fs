namespace Notifications.Domain
module Utils = 
    type MaybeBuilder() =
        member this.Bind(x, f) = 
            match x with
            | None -> None
            | Some a -> f a
        member this.Return(x) = 
            Some x

    let maybe = new MaybeBuilder()

    let strToSomeInt str =
        match System.Int32.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None

