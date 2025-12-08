module App.Item.Handler

open App
open App.Common.Model
open App.Common.Handler
open App.Item.Model
open Giraffe
open Serilog
open StarFederation.Datastar.DependencyInjection

let createItem : HttpHandler =
    fun next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let ds = ctx.GetService<IDatastarService>()
        let! signals = ctx.BindJsonAsync<{| x:int; y:int |}>()
        match signals.x, signals.y with
        | x, y when x >= 0 && x <= 10 && y >= 0 && y <= 10 ->
            Log.Information("👉 Creating item at ({x}, {y})", x, y)
            let id = Guid.create()
            let x = signals.x
            let y = signals.y
            itemStore.TryAdd((x, y), id) |> ignore
            do! dispatchEvent ds "item-created"
            return Some ctx
        | _ ->
            Log.Warning("⚠️ Invalid item coordinates ({x}, {y})", signals.x, signals.y)
            return! Successful.NO_CONTENT next ctx
    }
    
let getItemsPage : HttpHandler =
    fun next ctx -> task {
        let items =
            itemStore
            |> Seq.map (fun kvp ->
                let x, y = kvp.Key
                { id = kvp.Value; x = x; y = y })
        let page = View.itemsPage items
        let signals = { selectedNav = "items" }
        if ctx.IsDatastar then
            let ds = ctx.GetService<IDatastarService>()
            do! patchSignals ds signals
            do! patchElement ds page 
            do! pushUrl ds "/items"
            return Some ctx
        else
            return! renderPage (page, signals) next ctx
    }
    
let getItemsData : HttpHandler =
    fun _next ctx -> task {
        if not ctx.IsDatastar then failwith "Not a Datastar request"
        let items =
            itemStore
            |> Seq.map (fun kvp ->
                let x, y = kvp.Key
                { id = kvp.Value; x = x; y = y })
        let data = View.createChartData items
        let signals = {| _itemsChartData = data |}
        let ds = ctx.GetService<IDatastarService>()
        do! patchSignals ds signals
        return Some ctx
    }
    
let app : HttpHandler =
    choose [
        POST >=> createItem
        GET >=> choose [
            routex "(/?)" >=> getItemsPage
            route "/data" >=> getItemsData
        ]
    ]
    
