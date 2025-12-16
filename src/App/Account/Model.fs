module App.Account.Model

open App
open FSharp.Data.Adaptive
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
    
type Decider<'E,'S> =
    { state:'S
      evolve:'S -> 'E -> 'S
      decide:'S -> 'E list }
    
type Inputs =
    { deposits:Map<DateOnly,cval<decimal>>
      withdrawals:Map<DateOnly,cval<decimal>>
      apy:cval<decimal> }
    
module Account =
    
    type Status =
        | WaitingForDayStart
        | WaitingForYieldAccrual
        | WaitingForDeposit
        | WaitingForWithdrawal
        | WaitingForYieldReceipt
    module Status =
        let toString (state:Status) =
            match state with
            | WaitingForDayStart -> "WaitingForDayStart"
            | WaitingForYieldAccrual -> "WaitingForYieldAccrual"
            | WaitingForDeposit -> "WaitingForDeposit"
            | WaitingForWithdrawal -> "WaitingForWithdrawal"
            | WaitingForYieldReceipt -> "WaitingForYieldReceipt"

    type State =
        { observedDate:DateOnly
          inputs:Inputs
          status:Status
          lastYieldAccruedDate:DateOnly
          lastYieldReceivedDate:DateOnly
          yieldAccrued:aval<decimal>
          balance:aval<decimal> }
        
    type Event =
        | DayStarted of DateOnly
        | AmountDeposited of {| date:DateOnly; amount: aval<decimal> |}
        | AmountWithdrawn of {| date:DateOnly; amount: aval<decimal> |}
        | YieldAccrued of {| date:DateOnly; amount: aval<decimal> |}
        | YieldReceived of {| date:DateOnly; amount: aval<decimal> |}
    module Event =
        let toString (event:Event) =
            match event with
            | DayStarted _ -> "DayStarted"
            | AmountDeposited _ -> "AmountDeposited"
            | AmountWithdrawn _ -> "AmountWithdrawn"
            | YieldAccrued _ -> "YieldAccrued"
            | YieldReceived _ -> "YieldReceived"
            
    module Decider =
        let initial (startDate:DateOnly) (inputs:Inputs) =
            { observedDate = startDate
              inputs = inputs
              status = WaitingForDeposit
              lastYieldAccruedDate = startDate
              lastYieldReceivedDate = startDate
              yieldAccrued = !0m
              balance = !0m }
        let evolve state event =
            match event with
            | DayStarted date ->
                { state with
                    observedDate = date
                    status = WaitingForYieldAccrual }
            | YieldAccrued e ->
                { state with
                    yieldAccrued = state.yieldAccrued +. e.amount
                    status = WaitingForDeposit }
            | AmountDeposited e ->
                { state with
                    balance = state.balance +. e.amount
                    status = WaitingForWithdrawal }
            | AmountWithdrawn e ->
                let isFriday = state.observedDate.DayOfWeek = DayOfWeek.Friday
                let status =
                    if isFriday then WaitingForYieldReceipt
                    else WaitingForDayStart
                { state with
                    balance = state.balance -. e.amount
                    status = status }
            | YieldReceived e ->
                { state with
                    yieldAccrued = state.yieldAccrued -. e.amount
                    balance = state.balance +. e.amount
                    status = WaitingForDayStart }
            
        let decide (state:State) : Event list =
            match state.status with
            | WaitingForDayStart -> List.empty
            | WaitingForYieldAccrual ->
                let date = state.observedDate
                let days = DateOnly.daysBetween state.lastYieldAccruedDate date
                let dpy = state.inputs.apy /. !365m
                let yieldAccrued = state.balance *. dpy *. !(decimal days)
                [ YieldAccrued {| date = date; amount = yieldAccrued |} ]
            | WaitingForDeposit ->
                let date = state.observedDate
                let amount = state.inputs.deposits |> Map.tryFind date |> Option.fail $"Deposit {date} not found"
                [ AmountDeposited {| date = date; amount = amount |} ]
            | WaitingForWithdrawal ->
                let date = state.observedDate
                let amount = state.inputs.withdrawals |> Map.tryFind date |> Option.fail $"Withdrawal {date} not found"
                let amount = AVal.map2 min state.balance amount
                [ AmountWithdrawn {| date = date; amount = amount |} ]
            | WaitingForYieldReceipt ->
                let date = state.observedDate
                let yieldReceived = state.yieldAccrued |> AVal.map (fun v -> Math.Round(v, 2))
                [ YieldReceived {| date = date; amount = yieldReceived |} ]
        
        let create startDate inputs =
            let initialState = initial startDate inputs
            Decider.create initialState evolve decide

module Model =
            
    type Event =
        | DayStarted of DateOnly
        | AccountEvent of Account.Event
        | AccountObserved of Account.State
        
    module Event =
        let ofAccountEvent (e:Account.Event) = AccountEvent e
        let tryToAccountEvent = function
            | DayStarted date -> Some(Account.DayStarted date)
            | AccountEvent e -> Some e
            | AccountObserved _ -> None

    type Status =
        | WaitingForDayStart
        | WaitingForObservation
        | Running
        
    type State =
        { startDate: DateOnly
          endDate: DateOnly
          inputs: Inputs
          observedDate: DateOnly
          status: Status
          account: Account.State
          observations:Map<DateOnly,Account.State>
          events: Event list }
            
    module Decider =
        open DeciderOperators
        
        let initial startDate endDate inputs account : State =
            { startDate = startDate
              endDate = endDate
              inputs = inputs
              observedDate = startDate
              status = Running
              account = account
              observations = Map.empty
              events = List.empty }

        let evolve (state: State) (event: Event) : State =
            match event with
            | DayStarted date ->
                { state with
                    observedDate = date
                    status = Running }
            | AccountObserved account ->
                { state with
                    observations = state.observations |> Map.add state.observedDate account
                    status = WaitingForDayStart }
            | AccountEvent _ -> state

        let decide state : Event list =
            match state.status with
            | WaitingForDayStart ->
                let nextDate = state.observedDate.AddDays(1)
                if nextDate > state.endDate then
                    []
                else
                    [ DayStarted nextDate ]
            | Running ->
                match state.account.status with
                | Account.WaitingForDayStart ->
                    [ AccountObserved state.account ]
                | _ -> List.empty
            | _ -> List.empty
            
        let create startDate endDate inputs =
            let accountDecider = Account.Decider.create startDate inputs
            let accountDecider' =
                accountDecider
                |> Decider.adapt
                    Event.tryToAccountEvent
                    Event.ofAccountEvent
                    _.account
            let initialState = initial startDate endDate inputs accountDecider.state
            let decider = Decider.create initialState evolve decide
            (fun s a -> { s with account = a })
            <!> decider
            <*> accountDecider'
                
    let build startDate endDate inputs : State =
        Decider.create startDate endDate inputs
        |> Decider.run
        |> Decider.state
        
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
