module App.Time.Handler

open App
open App.Common.Model
open App.Common.Handler
open Giraffe
open StarFederation.Datastar.DependencyInjection
open System
open System.Threading.Tasks

let getTimePage : HttpHandler =
    fun next ctx -> task {
        let currentTime = DateTimeOffset.UtcNow
        let page = View.timePage currentTime
        let signals = { selectedNav = "time" }
        if ctx.IsDatastar then
            let ds = ctx.GetService<IDatastarService>()
            do! patchElement ds page
            do! patchSignals ds signals
            do! pushUrl ds "/time"
            return Some ctx
        else
            return! renderPage (page, signals) next ctx
    }
    
let getUpdatedTime : HttpHandler =
    fun _next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        while true do
            do! Task.Delay(1_000)
            let currentTime = DateTimeOffset.UtcNow
            let currentTimeElement = View.currentTimeElement currentTime
            do! patchElement ds currentTimeElement
        return Some ctx
    }
    
let app : HttpHandler =
    choose [
        GET >=> choose [
            routex "(/?)" >=> getTimePage
            route "/update" >=> getUpdatedTime
        ]
    ]
    
