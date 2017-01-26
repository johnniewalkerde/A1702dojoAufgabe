open NEventStore
open System

type IName = 
    abstract Name : string
type BaseEvent(login) = 
    member this.Login = login
type PasswordEvent(login, password) = 
    inherit BaseEvent(login)
    member this.Password = password
type RegisterEvent(login, name, password) =
    inherit PasswordEvent(login, password)
    interface IName with
        member this.Name = name
type ChangeNameEvent(login, name) =
    inherit BaseEvent(login)
    interface IName with
        member this.Name = name

let store = 
    Wireup
        .Init()
        .UsingInMemoryPersistence()
        .Build()
let userlogin = "UserLogin"
let stream = store.CreateStream(userlogin)
let processEvent event =
    let message = new EventMessage()
    message.Body <- event
    stream.Add message
    stream.CommitChanges (System.Guid.NewGuid())
let replayEvents() =
    let outStream = store.OpenStream(userlogin, 0, Int32.MaxValue)
    outStream.CommittedEvents |> Seq.iter (fun x -> printfn "%A" x.Body)

let checkCurrentPassword login password =
    let outStream = store.OpenStream(userlogin, 0, Int32.MaxValue)
    let events = outStream.CommittedEvents 
                |> Seq.filter (fun message ->
                    match box message.Body with
                    | :? PasswordEvent -> true
                    | _ -> false )
                |> Seq.map (fun e -> e.Body :?> PasswordEvent)
                |> Seq.filter (fun event -> event.Login = login)
    match events with
    | e when e |> Seq.length = 0 -> 
        false
    | e -> 
        let event = events |> Seq.last
        event.Password = password
let getName login =
    let outStream = store.OpenStream(userlogin, 0, Int32.MaxValue)
    let events = outStream.CommittedEvents 
                |> Seq.filter (fun message ->
                    match box message.Body with
                    | :? IName ->
                        let l = message.Body :?> BaseEvent
                        l.Login = login
                    | _ -> false )
                |> Seq.map (fun e -> e.Body :?> IName)
    match events with
    | e when e |> Seq.length = 0 -> 
        None
    | e -> 
        let event = events |> Seq.last
        Some event.Name
   
type UserLogin() =
    member this.Register login name password = 
        processEvent (new RegisterEvent(login, name, password))
    member this.CanLogin login password =
        checkCurrentPassword login password
    member this.ChangePassword login oldpassword newpassword =
        match (this.CanLogin login oldpassword) with
        | true -> 
            processEvent (new PasswordEvent(login, newpassword))
            true
        | _ -> false
    member this.ChangeName login password newname =
        match (this.CanLogin login password) with
        | true -> 
            processEvent (new ChangeNameEvent(login, newname))
            true
        | _ -> false

[<EntryPoint>]
let main argv = 
    let l = new UserLogin()

    let register login name password =
        l.Register login name password
        printfn "registered %s with name %s" login name
    let changepassword login oldp newp =
        match l.ChangePassword login oldp newp with
        | true -> printfn "changed password for %s" login
        | _ -> printfn "unable to change password for %s" login
    let canlogin login password =
        printfn "%10s with password %10s can login: %10b" login password (l.CanLogin login password)
    let changename login password name = 
        match l.ChangeName login password name with
        | true -> printfn "changed name for %s to %s" login name
        | _ -> printfn "unable to change name for %s" login
    let printname login =
        match getName login with
        | Some x -> printfn "login %s has name %s" login x
        | _ -> printfn "name for login %s is unknown" login

    register "a" "a" "111"
    register "b" "b" "222"
    canlogin "c" "333"
    canlogin "a" "111"
    canlogin "a" "222"
    canlogin "b" "111"
    canlogin "b" "222"
    changepassword "b" "222" "333"
    canlogin "b" "222"
    canlogin "b" "333"
    changepassword "b" "222" "333"
    canlogin "b" "222"
    printname "c"
    printname "b"
    changename "b" "333" "New name of b"
    printname "b"

    Console.ReadKey() |> ignore
    0
