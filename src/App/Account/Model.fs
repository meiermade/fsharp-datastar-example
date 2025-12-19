module App.Account.Model

open App
open AdaptiveOperators
open FSharp.Data.Adaptive
open System

type AccountId = AccountId of string

type Inputs =
    { startDate:DateOnly
      endDate:DateOnly
      checkingDeposits:Map<DateOnly,cval<decimal>>
      savingsTransfers:Map<DateOnly,cval<decimal>>
      savingsWithdrawals:Map<DateOnly,cval<decimal>>
      checkingApy:cval<decimal>
      savingsApy:cval<decimal> }
    
module Constants =
    let zero = !0m
    let dayCount = !365m
    
module Account =
    
    type AccountType =
        | Nominal
        | Real
        
    type Account =
        { id:AccountId
          accountType:AccountType }
    
    type State =
        { account:Account
          balance:aval<decimal> }
    module State =
        let initial account =
            { account = account
              balance = Constants.zero }
        
    type Event =
        | Deposited of {| amount:aval<decimal> |}
        | Withdrawn of {| amount:aval<decimal> |}
        
    module Evolver =
        let evolve (state:State) (event:Event) =
            match event with
            | Deposited e ->
                { state with
                    balance = state.balance +. e.amount }
            | Withdrawn e ->
                { state with
                    balance = state.balance -. e.amount }
                
        let create accountType =
            let initialState = State.initial accountType
            Evolver.create initialState evolve
            
module Observation =
    type Event =
        | DayStarted of {| date:DateOnly |}
        | AccountEvent of {| accountId:AccountId; event:Account.Event |}
    module Event =
        let ofAccountEvent (accountId, event) =
            AccountEvent {| accountId = accountId; event = event |}
        let tryToAccountEvent e =
            match e with
            | DayStarted _ -> None
            | AccountEvent e -> Some (e.accountId, e.event)
        
    type Command =
        | StartDay of {| date:DateOnly |}
        | AccrueYield of {| balanceAccountId:AccountId; revenueAccountId:AccountId; yieldAccountId:AccountId; apy:aval<decimal> |}
        | ReceiveYield of {| balanceAccountId:AccountId; yieldAccountId:AccountId |}
        | Transact of {| fromAccountId:AccountId; toAccountId:AccountId; amount:aval<decimal> |}
    module Command =
        let tryToAccountCommand _c = None
        
    type State =
        { observedDate:DateOnly
          accounts:Map<AccountId,Account.State> }
    module State =
        let initial startDate accounts =
            { observedDate = startDate
              accounts = accounts }
            
    module Decider =
        open DeciderOperators
        
        let evolve (state: State) (event: Event) : State =
            match event with
            | DayStarted e ->
                { state with
                    observedDate = e.date }
            | AccountEvent _ -> state

        let decide command state : Event list =
            match command with
            | StartDay c ->
                [ DayStarted {| date = c.date |} ]
            | AccrueYield c ->
                let balance = state.accounts[c.balanceAccountId].balance
                let amount = c.apy /. Constants.dayCount *. balance
                [ AccountEvent {| accountId = c.revenueAccountId; event = Account.Withdrawn {| amount = amount |} |}
                  AccountEvent {| accountId = c.yieldAccountId; event = Account.Deposited {| amount = amount |} |} ]
            | ReceiveYield c ->
                let amount = state.accounts[c.yieldAccountId].balance |> AVal.map (fun b -> Math.Round(b, 2))
                [ AccountEvent {| accountId = c.yieldAccountId; event = Account.Withdrawn {| amount = amount |} |}
                  AccountEvent {| accountId = c.balanceAccountId; event = Account.Deposited {| amount = amount |} |} ]
            | Transact c ->
                let fromAccount = state.accounts[c.fromAccountId]
                let amount =
                    match fromAccount.account.accountType with
                    | Account.Nominal -> c.amount
                    | Account.Real -> AVal.map2 min fromAccount.balance c.amount
                [ AccountEvent {| accountId = c.fromAccountId; event = Account.Withdrawn {| amount = amount |} |}
                  AccountEvent {| accountId = c.toAccountId; event = Account.Deposited {| amount = amount |} |} ]
            
        let create startDate (accounts:Account.Account list) =
            let accountEvolvers =
                accounts
                |> Seq.map (fun a -> a.id, Account.Evolver.create a)
                |> Map.ofSeq
            let accountsDecider =
                Evolver.many accountEvolvers
                |> Decider.adaptEvolver
                    Event.tryToAccountEvent
                    _.accounts
            let initialState = State.initial startDate accountsDecider.initialState
            let observationDecider = Decider.create initialState evolve decide
            (fun o a -> { o with accounts = a })
            <!> observationDecider
            <*> accountsDecider
            
module Model =
    let revenueAccountId = AccountId "Revenue"
    let expenseAccountId = AccountId "Expense"
    let checkingAccountId = AccountId "Checking"
    let checkingYieldAccountId = AccountId "Checking Yield"
    let savingsAccountId = AccountId "Savings"
    let savingsYieldAccountId = AccountId "Savings Yield"
    let accounts:Account.Account list =
        [ { id = revenueAccountId; accountType = Account.Nominal }
          { id = expenseAccountId; accountType = Account.Nominal }
          { id = checkingAccountId; accountType = Account.Real }
          { id = checkingYieldAccountId; accountType = Account.Nominal }
          { id = savingsAccountId; accountType = Account.Real }
          { id = savingsYieldAccountId; accountType = Account.Nominal } ]
    
    type State =
        { inputs:Inputs
          observation:Observation.State
          observations:Map<DateOnly,Observation.State> }
    module State =
        let initial inputs observation =
            { inputs = inputs
              observation = observation
              observations = Map.empty }
        
    type Event =
        | Observed of {| observation:Observation.State |}
        | ObservationEvent of {| event:Observation.Event |}
    module Event =
        let tryToObservationEvent = function
            | Observed _ -> None
            | ObservationEvent e -> Some e.event
        let ofObservationEvent e =
            ObservationEvent {| event = e |}

    type Command =
        | Observe
        | ObservationCommand of Observation.Command
    module Command =
        let tryToObservationCommand = function
            | Observe -> None
            | ObservationCommand c -> Some c
        
    module Decider =
        open DeciderOperators
        
        let evolve (state:State) (event:Event) =
            match event with
            | Observed e -> { state with observations = state.observations |> Map.add e.observation.observedDate e.observation }
            | ObservationEvent _ -> state
            
        let decide command state =
            match command with
            | Observe -> [ Observed {| observation = state.observation |} ]
            | ObservationCommand _c -> List.empty
            
        let create inputs =
            let observationDecider =
                Observation.Decider.create inputs.startDate accounts
                |> Decider.adapt
                    Command.tryToObservationCommand
                    Event.tryToObservationEvent
                    Event.ofObservationEvent
                    _.observation
            let initialState = State.initial inputs observationDecider.initialState
            let modelDecider = Decider.create initialState evolve decide
            (fun m o -> { m with observation = o })
            <!> modelDecider
            <*> observationDecider
            
    let private startDay date =
        {| date = date |}
        |> Observation.StartDay
        |> ObservationCommand
            
    let private accrueCheckingYield checkingApy =
        {| balanceAccountId = checkingAccountId
           revenueAccountId = revenueAccountId
           yieldAccountId = checkingYieldAccountId
           apy = checkingApy |}
        |> Observation.AccrueYield
        |> ObservationCommand
        
    let private receiveCheckingYield =
        {| yieldAccountId = checkingYieldAccountId
           balanceAccountId = checkingAccountId |}
        |> Observation.ReceiveYield
        |> ObservationCommand
        
    let private accrueSavingsYield savingsApy =
        {| balanceAccountId = savingsAccountId
           revenueAccountId = revenueAccountId
           yieldAccountId = savingsYieldAccountId
           apy = savingsApy |}
        |> Observation.AccrueYield
        |> ObservationCommand
        
    let private receiveSavingsYield =
        {| yieldAccountId = savingsYieldAccountId
           balanceAccountId = savingsAccountId |}
        |> Observation.ReceiveYield
        |> ObservationCommand
        
    let private depositToChecking amount =
        {| fromAccountId = revenueAccountId
           toAccountId = checkingAccountId
           amount = amount |}
        |> Observation.Transact
        |> ObservationCommand
        
    let private transferToSavings amount =
        {| fromAccountId = checkingAccountId
           toAccountId = savingsAccountId
           amount = amount |}
        |> Observation.Transact
        |> ObservationCommand
        
    let private withdrawFromSavings amount =
        {| fromAccountId = savingsAccountId
           toAccountId = expenseAccountId
           amount = amount |}
        |> Observation.Transact
        |> ObservationCommand
        
    let private dailyCommands inputs date =
        [ startDay date
          accrueCheckingYield inputs.checkingApy
          accrueSavingsYield inputs.savingsApy
          if date.DayOfWeek = DayOfWeek.Friday then
              receiveCheckingYield
              receiveSavingsYield
          depositToChecking inputs.checkingDeposits[date]
          transferToSavings inputs.savingsTransfers[date]
          withdrawFromSavings inputs.savingsWithdrawals[date]
          Observe ]
                
    let build (inputs:Inputs) : State =
        let decider = Decider.create inputs
        let dates = DateOnly.range inputs.startDate inputs.endDate
        (decider.initialState, dates)
        ||> Seq.fold (fun state date ->
            let commands = dailyCommands inputs date
            (state, commands)
            ||> Seq.fold (fun state command ->
                let events = decider.decide command state
                (state, events) ||> Seq.fold decider.evolve))
        
let startDate = DateOnly(2025, 1, 1)
let endDate = DateOnly(2025, 1, 31)
let dates = DateOnly.range startDate endDate
let checkingDeposits = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList
let savingsTransfers = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList
let savingsWithdrawals = dates |> List.map (fun d -> (d, cval 0m)) |> Map.ofList

let inputs:Inputs =
    { startDate = startDate
      endDate = endDate
      checkingDeposits = checkingDeposits
      savingsTransfers = savingsTransfers
      savingsWithdrawals = savingsWithdrawals
      checkingApy = cval 0.01m
      savingsApy = cval 0.02m }
    
let model = Model.build inputs

let updateCheckingDeposit date amount =
    transact(fun () -> model.inputs.checkingDeposits[date].Value <- amount)
    
let updateSavingsTransfer date amount =
    transact(fun () -> model.inputs.savingsTransfers[date].Value <- amount)
    
let updateSavingsWithdrawal date amount =
    transact(fun () -> model.inputs.savingsWithdrawals[date].Value <- amount)
    
let updateCheckingApy apy =
    transact(fun () -> model.inputs.checkingApy.Value <- apy)
    
let updateSavingsApy apy =
    transact(fun () -> model.inputs.savingsApy.Value <- apy)
