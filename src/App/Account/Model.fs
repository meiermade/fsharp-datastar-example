module App.Account.Model

open App
open FSharp.Data.Adaptive
open Serilog
open System

[<AutoOpen>]
module AdaptiveOperators =
    let ( +. ) a b = AVal.map2 (+) a b
    let ( -. ) a b = AVal.map2 (-) a b
    let ( *. ) a b = AVal.map2 (*) a b
    let ( /. ) a b = AVal.map2 (/) a b
    let (!) a = AVal.constant a
    let (!~) a = cval a
    let (!!) a = AVal.force a
        
type Inputs =
    { deposits:Map<DateOnly,cval<decimal>>
      withdrawals:Map<DateOnly,cval<decimal>>
      apy:cval<decimal> }
    
type AccountStatus =
    | WaitingForDayStart
    | WaitingForYieldAccrual
    | WaitingForDeposit
    | WaitingForWithdrawal
    | WaitingForYieldReceipt
module AccountStatus =
    let toString (state:AccountStatus) =
        match state with
        | WaitingForDayStart -> "WaitingForDayStart"
        | WaitingForYieldAccrual -> "WaitingForYieldAccrual"
        | WaitingForDeposit -> "WaitingForDeposit"
        | WaitingForWithdrawal -> "WaitingForWithdrawal"
        | WaitingForYieldReceipt -> "WaitingForYieldReceipt"
        
type Account =
    { observedDate:DateOnly
      status:AccountStatus
      apy:aval<decimal>
      lastYieldAccruedDate:DateOnly
      lastYieldReceivedDate:DateOnly
      yieldAccrued:aval<decimal>
      balance:aval<decimal> }
    
type AccountEvent =
    | DayStarted of DateOnly
    | AmountDeposited of {| date:DateOnly; amount: aval<decimal> |}
    | AmountWithdrawn of {| date:DateOnly; amount: aval<decimal> |}
    | YieldAccrued of {| date:DateOnly; amount: aval<decimal> |}
    | YieldReceived of {| date:DateOnly; amount: aval<decimal> |}
module AccountEvent =
    let toString (event:AccountEvent) =
        match event with
        | DayStarted _ -> "DayStarted"
        | AmountDeposited _ -> "AmountDeposited"
        | AmountWithdrawn _ -> "AmountWithdrawn"
        | YieldAccrued _ -> "YieldAccrued"
        | YieldReceived _ -> "YieldReceived"
        
type AccountCommand =
    | StartDay of DateOnly
    | AccrueYield of DateOnly
    | Deposit of DateOnly * aval<decimal>
    | Withdraw of DateOnly * aval<decimal>
    | ReceiveYield of DateOnly
    
module Account =
    let initial (startDate:DateOnly) apy =
        { observedDate = startDate
          status = WaitingForYieldAccrual
          apy = apy
          lastYieldAccruedDate = startDate
          lastYieldReceivedDate = startDate
          yieldAccrued = !0m
          balance = !0m }
    let evolve account event =
        match event with
        | DayStarted date ->
            { account with
                observedDate = date
                status = WaitingForYieldAccrual }
        | YieldAccrued e ->
            { account with
                yieldAccrued = account.yieldAccrued +. e.amount
                status = WaitingForDeposit }
        | AmountDeposited e ->
            { account with
                balance = account.balance +. e.amount
                status = WaitingForWithdrawal }
        | AmountWithdrawn e ->
            let status =
                if account.observedDate.DayOfWeek = DayOfWeek.Friday
                then WaitingForYieldReceipt
                else WaitingForDayStart
            { account with
                balance = account.balance -. e.amount
                status = status }
        | YieldReceived e ->
            { account with
                yieldAccrued = account.yieldAccrued -. e.amount
                balance = account.balance +. e.amount
                status = WaitingForDayStart }
        
    let fold:Account -> AccountEvent seq -> Account = Seq.fold evolve
    
    let decide command (account:Account) : AccountEvent list =
        match account.status, command with
        | WaitingForDayStart, StartDay date ->
            [ DayStarted date ]
        | WaitingForYieldAccrual, AccrueYield date ->
            let days = DateOnly.daysBetween account.lastYieldAccruedDate date
            let dpy = account.apy /. !365m
            let yieldAccrued = account.balance *. dpy *. !(decimal days)
            [ YieldAccrued {| date = date; amount = yieldAccrued |} ]
        | WaitingForDeposit, Deposit (date, amount) ->
            [ AmountDeposited {| date = date; amount = amount |} ]
        | WaitingForWithdrawal, Withdraw (date, amount) ->
            let amount = AVal.map2 min account.balance amount
            [ AmountWithdrawn {| date = date; amount = amount |} ]
        | WaitingForYieldReceipt, ReceiveYield date  ->
            let yieldReceived = account.yieldAccrued |> AVal.map (fun v -> Math.Round(v, 2))
            [ YieldReceived {| date = date; amount = yieldReceived |} ]
        | _ -> List.empty
            
    let next (inputs:Inputs) (account:Account) : AccountCommand list =
        match account.status with
        | WaitingForDayStart -> List.empty
        | WaitingForYieldAccrual ->
            [ AccrueYield account.observedDate ]
        | WaitingForDeposit ->
            let date = account.observedDate
            let amount = inputs.deposits |> Map.tryFind date |> Option.fail $"Deposit {date} not found"
            [ Deposit (date, amount) ]
        | WaitingForWithdrawal ->
            let date = account.observedDate
            let amount = inputs.withdrawals |> Map.tryFind date |> Option.fail $"Withdrawal {date} not found"
            [ Withdraw (date, amount) ]
        | WaitingForYieldReceipt ->
            [ ReceiveYield account.observedDate ]
            
type ModelEvent =
    | DayStarted of DateOnly
    | AccountEvent of AccountEvent
    | AccountObserved of Account
    
type ModelCommand =
    | StartDay of DateOnly
    | AccountCommand of AccountCommand
    | ObserveAccount of DateOnly

type ModelStatus =
    | WaitingForDayStart
    | WaitingForAccount
    | WaitingForObservation
    | Complete
    
type Model =
    { startDate: DateOnly
      endDate: DateOnly
      inputs: Inputs
      observedDate: DateOnly
      status: ModelStatus
      account: Account
      observations:Map<DateOnly,Account>
      events: ModelEvent list }
        
module Model =
    let initial (startDate:DateOnly, endDate:DateOnly, inputs:Inputs) : Model =
        { startDate = startDate
          endDate = endDate
          inputs = inputs
          observedDate = startDate
          status = WaitingForAccount
          account = Account.initial startDate inputs.apy
          observations = Map.empty
          events = List.empty }

    let evolve (model:Model) (event:ModelEvent) : Model =
        match event with
        | DayStarted date ->
            { model with
                observedDate = date
                status = WaitingForAccount }
        | AccountEvent e ->
            let account = Account.evolve model.account e
            let status =
                match account.status with
                | AccountStatus.WaitingForDayStart -> WaitingForObservation
                | _ -> WaitingForAccount
            { model with
                account = account
                status = status }
        | AccountObserved account ->
            let status =
                if model.observedDate = model.endDate
                then Complete
                else WaitingForDayStart
            { model with
                observations = model.observations |> Map.add account.observedDate account
                status = status }

    let decide command model : ModelEvent list =
        match model.status, command with
        | WaitingForDayStart, StartDay date ->
            [ DayStarted date ]
        | WaitingForAccount, AccountCommand cmd ->
            Account.decide cmd model.account
            |> List.map AccountEvent
        | WaitingForObservation, ObserveAccount date ->
            if model.observations.ContainsKey date
            then List.empty
            else [ AccountObserved model.account ]
        | _ -> List.empty
            
    let next model: ModelCommand list =
        match model with
        | { status = Complete } -> List.empty
        | { status = WaitingForDayStart } ->
            let nextDate = model.observedDate.AddDays(1)
            [ StartDay nextDate
              AccountCommand (AccountCommand.StartDay nextDate) ]
        | { status = WaitingForAccount } ->
            Account.next model.inputs model.account
            |> List.map AccountCommand
        | { status = WaitingForObservation } ->
            [ ObserveAccount model.observedDate ]
            
    let fold = Seq.fold evolve
        
    let rec run model commands : Model =
        match commands with
        | [] ->
            match next model with
            | [] -> model
            | nextCommands ->
                run model nextCommands
        | command :: rest ->
            let events = decide command model
            let newModel = fold model events
            run newModel rest
            
    let build (startDate:DateOnly) (endDate:DateOnly) inputs : Model =
        let model = initial (startDate, endDate, inputs)
        run model []
        
let startDate = DateOnly(2025, 1, 1)
let endDate = DateOnly(2025, 1, 31)
let dates = DateOnly.range startDate endDate
let deposits = dates |> List.map (fun d -> (d, cval 100m)) |> Map.ofList
let withdrawals = dates |> List.map (fun d -> (d, cval 50m)) |> Map.ofList
let inputs:Inputs =
    { deposits = deposits
      withdrawals = withdrawals
      apy = cval 0.05m }
let model = Model.build startDate endDate inputs
let observations = model.observations |> Map.toSeq |> Seq.map (fun (d, o) -> (d, (!!o.balance))) |> Seq.toList

let updateDeposit date amount =
    transact(fun () -> model.inputs.deposits[date].Value <- amount)
    
let updateWithdrawal date amount =
    transact(fun () -> model.inputs.withdrawals[date].Value <- amount)
    
let updateApy apy =
    transact(fun () -> model.inputs.apy.Value <- apy)
