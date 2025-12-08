module App.Common.Handler

open App.Common.Model
open FSharp.ViewEngine
open Giraffe
open StarFederation.Datastar.DependencyInjection

let inline patchSignals (ds:IDatastarService) (signals:'T) = task {
    do! ds.PatchSignalsAsync(signals)
}

let patchElement (ds:IDatastarService) (element:Element) = task {
    let html = Element.render element
    do! ds.PatchElementsAsync(html)
}

let dispatchEvent (ds:IDatastarService) (name:string) = task {
    // language=javascript
    let js = $$"""window.dispatchEvent(new CustomEvent('{{ name }}', { bubbles: true }))"""
    do! ds.ExecuteScriptAsync(js)
}

let pushUrl (ds:IDatastarService) (url:string) = task {
    // language=javascript
    let js = $$"""window.history.pushState({}, '', '{{ url }}')"""
    do! ds.ExecuteScriptAsync(js)
}

let renderPage (page:Element, signals:Signals) : HttpHandler =
    fun next ctx -> task {
        let html = View.layout (page, signals) |> Element.render
        return! htmlString html next ctx
    }
