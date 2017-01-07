namespace FakeDb
open System.Collections.Generic
type Person = {
  Id : int 
  Name : string
  Age : int
  Email : string
}
module Db =
  let private peopleStorage = new Dictionary<int, Person>()
  let getPeople () =
    peopleStorage.Values |> Seq.map (fun p -> p)

  let createPerson person = 
    printfn "Person in Create is %A" person
    let id = peopleStorage.Values.Count + 1
    let newPerson = {
        Id = id
        Name = person.Name
        Age = person.Age
        Email = person.Email
    }
    match newPerson.Email with 
        | null -> printfn "WTF! email is null"
        | s  -> printfn "honkey dory"
        | _ -> printfn "what is it?"
    peopleStorage.Add(id, newPerson)
    newPerson

  let GetById id = 
    printfn "Trying to get by id"
    let (matched, person) = peopleStorage.TryGetValue id
    match matched with 
      | true -> Some person 
      | false -> None

