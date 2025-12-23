module App.Account.Model

open App
open AdaptiveOperators
open FSharp.Data.Adaptive
open System

type AccountId = AccountId of string

type Inputs =
    { startDate:DateOnly
      endDate:DateOnly
      paycheck:Map<DateOnly,cval<decimal>>
      rent:Map<DateOnly,cval<decimal>>
      checkingApy:cval<decimal>
      savingsApy:cval<decimal> }
    
module Constants =
    let zero = !0m
    let dayCount = !365m
    
// TODO: create yield bearing account that has two accounts as reactor to capitalize on schedule
module Accounts =
    
    type Transaction =
        { description:string
          fromAccountId:AccountId
          toAccountId:AccountId
          amount:aval<decimal> }
    
    type State = Map<AccountId,aval<decimal>>
    module State =
        let initial = Map.empty

    type Event =
        | Transacted of Transaction
        
    module Evolver =
        let evolve (state:State) (event:Event) =
            match event with
            | Transacted e ->
                let fromAccountBalance = state |> Map.tryFind e.fromAccountId |> Option.defaultValue !0m
                let toAccountBalance = state |> Map.tryFind e.toAccountId |> Option.defaultValue !0m
                let newFromAccountBalance = fromAccountBalance -. e.amount
                let newToAccountBalance = toAccountBalance +. e.amount
                state
                |> Map.add e.fromAccountId newFromAccountBalance
                |> Map.add e.toAccountId newToAccountBalance
                
        let create = Evolver.create State.initial evolve
            
module Observation =
    
    type Event =
        | DayStarted of DateOnly
        | AccountsEvent of Accounts.Event
    module Event =
        let tryToAccountsEvent e =
            match e with
            | AccountsEvent e -> Some e
            | _ -> None
            
    type Command =
        | StartDay of DateOnly
        | AccrueYield of
            {| balanceAccountId:AccountId
               externalAccountId:AccountId
               yieldAccountId:AccountId
               apy:aval<decimal> |}
        | CapitalizeYield of
            {| yieldAccountId:AccountId
               balanceAccountId:AccountId |}
        | Transact of Accounts.Transaction
            
    type State =
        { observedDate:DateOnly
          accounts:Accounts.State }
    module State =
        let initial startDate accounts =
            { observedDate = startDate
              accounts = accounts }
            
    module Decider =
        open DeciderOperators
        
        let evolve (state:State) (event:Event) : State =
            match event with
            | DayStarted date ->
                { state with
                    observedDate = date }
            | AccountsEvent _ -> state

        let decide (command:Command) (state:State) =
            match command with
            | StartDay date ->
                [ DayStarted date ]
            | AccrueYield c ->
                let balance = state.accounts |> Map.tryFind c.balanceAccountId |> Option.defaultValue !0m
                let amount = c.apy /. Constants.dayCount *. balance
                let transacted =
                    Accounts.Transacted
                        { description = "yield_accrued"
                          fromAccountId = c.externalAccountId
                          toAccountId = c.yieldAccountId
                          amount = amount }
                [ AccountsEvent transacted ]
            | CapitalizeYield c ->
                let roundDown a = floor (a * 100m) / 100m
                let roundDown' = AVal.map roundDown
                let amount =
                    state.accounts
                    |> Map.tryFind c.yieldAccountId
                    |> Option.map roundDown'
                    |> Option.defaultValue !0m
                let transacted =
                    Accounts.Transacted
                        { description = "yield_capitalized"
                          fromAccountId = c.yieldAccountId
                          toAccountId = c.balanceAccountId
                          amount = amount }
                [ AccountsEvent transacted ]
            | Transact t ->
                [ AccountsEvent(Accounts.Transacted t) ]
                
        let create startDate =
            let accountsDecider =
                Accounts.Evolver.create
                |> Decider.adaptEvolver
                    Event.tryToAccountsEvent
                    _.accounts
            let initialState = State.initial startDate accountsDecider.initialState
            let observationDecider = Decider.create initialState evolve decide
            (fun o a -> { o with accounts = a })
            <!> observationDecider
            <*> accountsDecider

module Model =
    let externalAccountId = AccountId "External"
    let expenseAccountId = AccountId "Expense"
    let checkingAccountId = AccountId "Checking"
    let checkingYieldAccountId = AccountId "Checking Yield"
    let savingsAccountId = AccountId "Savings"
    let savingsYieldAccountId = AccountId "Savings Yield"
    
    type Event =
        | Observed of Observation.State
        | ObservationEvent of Observation.Event
    module Event =
        let tryToObservationEvent = function
            | Observed _ -> None
            | ObservationEvent e -> Some e
        let ofObservationEvent e =
            ObservationEvent e

    type Command =
        | StartDay of DateOnly
        | AccrueCheckingYield of apy:aval<decimal>
        | AccrueSavingsYield of apy:aval<decimal>
        | CapitalizeCheckingYield
        | CapitalizeSavingsYield
        | DepositPaycheck of amount:aval<decimal>
        | PayRent of amount:aval<decimal>
        | TransferCheckingYield of amount:aval<decimal>
        | Observe
    module Command =
        let tryToObservationCommand = function
            | StartDay date -> Some(Observation.StartDay date)
            | AccrueCheckingYield apy ->
                Observation.AccrueYield
                    {| balanceAccountId = checkingAccountId
                       yieldAccountId = checkingYieldAccountId
                       externalAccountId = externalAccountId
                       apy = apy |}
                |> Some
            | AccrueSavingsYield apy ->
                Observation.AccrueYield
                    {| balanceAccountId = savingsAccountId
                       yieldAccountId = savingsYieldAccountId
                       externalAccountId = externalAccountId
                       apy = apy |}
                |> Some
            | CapitalizeCheckingYield ->
                Observation.CapitalizeYield
                    {| yieldAccountId = checkingYieldAccountId
                       balanceAccountId = checkingAccountId |}
                |> Some
            | CapitalizeSavingsYield ->
                Observation.CapitalizeYield
                    {| yieldAccountId = savingsYieldAccountId
                       balanceAccountId = savingsAccountId |}
                |> Some
            | TransferCheckingYield amount ->
                Observation.Transact
                    { description = "transfer_checking_yield"
                      fromAccountId = checkingAccountId
                      toAccountId = savingsAccountId
                      amount = amount }
                |> Some
            | DepositPaycheck amount ->
                Observation.Transact
                    { description = "paycheck"
                      fromAccountId = externalAccountId
                      toAccountId = checkingAccountId
                      amount = amount }
                |> Some
            | PayRent amount ->
                Observation.Transact
                    { description = "rent"
                      fromAccountId = checkingAccountId
                      toAccountId = externalAccountId
                      amount = amount }
                |> Some
            | Observe -> None

    type State =
        { inputs:Inputs
          observation:Observation.State
          observations:Map<DateOnly,Observation.State> }
    module State =
        let initial inputs observation =
            { inputs = inputs
              observation = observation
              observations = Map.empty }
        
    module Reactor =
        open ReactorOperators
        
        let evolve (state:State) (event:Event) =
            match event with
            | Observed o ->
                { state with observations = state.observations |> Map.add o.observedDate o }
            | _ -> state
                
        let decide command state =
            match command with
            | Observe ->
                [ Observed state.observation ]
            | _ -> List.empty
            
        let resume state =
            let observedDate = state.observation.observedDate
            if state.observations |> Map.containsKey observedDate then
                if observedDate = state.inputs.endDate then
                    []
                else
                    let nextDate = observedDate.AddDays 1
                    [ StartDay nextDate ]
            else
                [ Observe ]
                
        let react state event =
            match event with
            | ObservationEvent(Observation.DayStarted d) ->
                [ AccrueCheckingYield state.inputs.checkingApy
                  AccrueSavingsYield state.inputs.savingsApy
                  if d.DayOfWeek = DayOfWeek.Friday then
                    CapitalizeCheckingYield
                    CapitalizeSavingsYield
                  DepositPaycheck state.inputs.paycheck[d]
                  PayRent state.inputs.rent[d] ]
            | ObservationEvent(Observation.AccountsEvent(Accounts.Transacted t)) ->
                match t.description with
                | "yield_capitalized" when t.toAccountId = checkingAccountId ->
                    [ TransferCheckingYield t.amount ]
                | _ -> List.empty
            | Observed _ -> List.empty
            
        let create inputs =
            let observationReactor =
                Observation.Decider.create inputs.startDate
                |> Reactor.adaptDecider
                    Command.tryToObservationCommand
                    Event.tryToObservationEvent
                    Event.ofObservationEvent
                    _.observation
            let initialState = State.initial inputs observationReactor.initialState
            let modelReactor = Reactor.create initialState evolve decide resume react
            (fun m o -> { m with observation = o })
            <!> modelReactor
            <*> observationReactor
            
    // TODO: add resume to reactor so we can drive to terminal state
    let build (inputs:Inputs) : State =
        let reactor = Reactor.create inputs
        Reactor.run [ StartDay inputs.startDate ] reactor
        
let startDate = DateOnly(2025, 1, 1)
let endDate = DateOnly(2025, 1, 31)
let dates = DateOnly.range startDate endDate
let paycheck = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList
let rent = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList
let savingsWithdrawals = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList

let inputs:Inputs =
    { startDate = startDate
      endDate = endDate
      paycheck = paycheck
      rent = rent
      checkingApy = cval 0.1m
      savingsApy = cval 0.2m }
    
let model = Model.build inputs

let updatePaycheck date amount =
    transact(fun () -> model.inputs.paycheck[date].Value <- amount)
    
let updateRent date amount =
    transact(fun () -> model.inputs.rent[date].Value <- amount)
    
let updateCheckingApy apy =
    transact(fun () -> model.inputs.checkingApy.Value <- apy)
    
let updateSavingsApy apy =
    transact(fun () -> model.inputs.savingsApy.Value <- apy)
