module App.SimpleAccount.Model

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
    
type Inputs =
    { deposits:Map<DateOnly,cval<decimal>>
      withdrawals:Map<DateOnly,cval<decimal>>
      apy:cval<decimal> }
module Inputs =
    let initial =
        { deposits = Map.empty
          withdrawals = Map.empty
          apy = cval 0m }
    let create (dates:DateOnly list) =
        (initial, dates)
        ||> Seq.fold (fun i d ->
            { i with
                deposits = i.deposits |> Map.add d (cval 0m)
                withdrawals = i.withdrawals |> Map.add d (cval 0m) })
    
type Account =
    { yieldAccrued:Map<DateOnly,aval<decimal>>
      yieldReceived:Map<DateOnly,aval<decimal>>
      yieldBalance:Map<DateOnly,aval<decimal>>
      balance:Map<DateOnly,aval<decimal>> }
    
module Account =
    let dayCount = !365m
    let initialAccount =
        { yieldAccrued = Map.empty
          yieldReceived = Map.empty
          yieldBalance = Map.empty
          balance = Map.empty }
    let create (dates:DateOnly list) (inputs:Inputs) =
        (initialAccount, dates)
        ||> Seq.fold (fun a d ->
            let prevBalance =
                match a.balance |> Map.tryFind (d.AddDays -1) with
                | Some b -> b
                | None -> !0m
            let prevYieldBalance =
                match a.yieldBalance |> Map.tryFind (d.AddDays -1) with
                | Some y -> y
                | None -> !0m
            let dpy = inputs.apy /. dayCount
            let yieldAccrued = prevBalance *. dpy
            let yieldReceived =
                if d.DayOfWeek = DayOfWeek.Friday then prevYieldBalance +. yieldAccrued
                else !0m
            let yieldBalance = prevYieldBalance +. yieldAccrued -. yieldReceived
            let balance = prevBalance +. inputs.deposits[d] -. inputs.withdrawals[d] +. yieldReceived
            { yieldAccrued = a.yieldAccrued |> Map.add d yieldAccrued
              yieldReceived = a.yieldReceived |> Map.add d yieldReceived
              yieldBalance = a.yieldBalance |> Map.add d yieldBalance
              balance = a.balance |> Map.add d balance })
        
type Model =
    { dates: DateOnly list
      inputs: Inputs
      account: Account }
module Model =
    let startDate = DateOnly(2025, 1, 1)
    let endDate = DateOnly(2025, 1, 31)
    let dates = DateOnly.range startDate endDate |> Seq.toList
    let inputs = Inputs.create dates
    let account = Account.create dates inputs
    let model = { dates = dates; inputs = inputs; account = account }

    let updateDeposit date amount =
        transact(fun () -> inputs.deposits[date].Value <- amount)
        
    let updateWithdrawal date amount =
        transact(fun () -> inputs.withdrawals[date].Value <- amount)
        
    let updateApy apy =
        transact(fun () -> inputs.apy.Value <- apy)
