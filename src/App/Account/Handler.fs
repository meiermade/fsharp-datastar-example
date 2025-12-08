module App.Account.Handler

open App
open App.Common.Model
open App.Common.Handler
open Giraffe
open Serilog
open StarFederation.Datastar.DependencyInjection
open System

let getAccountPage : HttpHandler =
    fun next ctx -> task {
        let page = View.accountPage Model.model
        let signals = { selectedNav = "account" }
        if ctx.IsDatastar then
            let ds = ctx.GetService<IDatastarService>()
            do! patchElement ds page
            do! patchSignals ds { selectedNav = "account" }
            do! pushUrl ds "/account"
            return Some ctx
        else
            return! renderPage (page, signals) next ctx
    }
    
let updateDeposit : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:decimal |}>()
        Log.Information("👉 Updating deposit {date} to {amount}", signals.date, signals.amount)
        Model.updateDeposit signals.date signals.amount
        return! getAccountPage next ctx
    }
    
let updateWithdrawal : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:decimal |}>()
        Log.Information("👉 Updating withdrawal {date} to {amount}", signals.date, signals.amount)
        Model.updateWithdrawal signals.date signals.amount
        return! getAccountPage next ctx
    }
    
let updateApy : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| apy:decimal |}>()
        Log.Information("👉 Updating APY to {apy}", signals.apy)
        Model.updateApy signals.apy
        return! getAccountPage next ctx
    }
    
let app : HttpHandler =
    choose [
        routex "(/?)" >=> GET >=> getAccountPage
        route "/apy" >=> POST >=> updateApy
        route "/deposit" >=> POST >=> updateDeposit
        route "/withdrawal" >=> POST >=> updateWithdrawal
    ]
