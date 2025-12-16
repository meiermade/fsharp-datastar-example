module App.Index.Handler

open App
open App.Common.Model
open App.Common.Handler
open Giraffe
open StarFederation.Datastar.DependencyInjection

let getHomePage : HttpHandler =
    fun next ctx -> task {
        let page = View.homePage
        let signals = { selectedNav = "home" }
        if ctx.IsDatastar then
            let ds = ctx.GetService<IDatastarService>()
            do! patchElement ds page
            do! patchSignals ds signals
            do! pushUrl ds "/"
            return Some ctx
        else
            return! renderPage (page, signals) next ctx
    }
    
let app:HttpHandler =
    choose [
        route "/" >=> getHomePage
        subRoute "/items" Item.Handler.app
        subRoute "/account" Account.Handler.app
        subRoute "/simple-account" SimpleAccount.Handler.app
        subRoute "/time" Time.Handler.app
    ]
