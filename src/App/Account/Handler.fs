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
    
let updatePaycheck : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:decimal |}>()
        Log.Information("👉 Updating paycheck {date} to {amount}", signals.date, signals.amount)
        Model.updatePaycheck signals.date signals.amount
        return! getAccountPage next ctx
    }
    
let updateRent : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:decimal |}>()
        Log.Information("👉 Updating rent {date} to {amount}", signals.date, signals.amount)
        Model.updateRent signals.date signals.amount
        return! getAccountPage next ctx
    }
    
let updateCheckingApy : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| checkingApy:decimal |}>()
        Log.Information("👉 Updating APY to {apy}", signals.checkingApy)
        Model.updateCheckingApy signals.checkingApy
        return! getAccountPage next ctx
    }
    
let updateSavingsApy : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ds.ReadSignalsAsync<{| savingsApy:decimal |}>()
        Log.Information("👉 Updating APY to {apy}", signals.savingsApy)
        Model.updateSavingsApy signals.savingsApy
        return! getAccountPage next ctx
    }
    
let app : HttpHandler =
    choose [
        routex "(/?)" >=> GET >=> getAccountPage
        route "/checking-apy" >=> POST >=> updateCheckingApy
        route "/savings-apy" >=> POST >=> updateSavingsApy
        route "/paycheck" >=> POST >=> updatePaycheck
        route "/rent" >=> POST >=> updateRent
    ]
