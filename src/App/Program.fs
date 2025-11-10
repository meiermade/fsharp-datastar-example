open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Serilog
open System.Text.Json
open System.Collections.Concurrent

type Item =
    { x:int
      y:int }
        
let itemStore = ConcurrentDictionary<Item,bool>()
for x in 1 .. 3 do
    let item = { x = x; y = x }
    itemStore.TryAdd(item, true) |> ignore

module Json =
    let serialize (value:'T) = JsonSerializer.Serialize(value)
    
module Js =
    let serialize (value:'T) = JsonSerializer.Serialize(value).Replace("\"", "'")

module Model =
    [<CLIMutable>]
    type ItemForm =
        { x:int
          y:int }

module View =
    open FSharp.ViewEngine
    open type Html
    
    let navLink (id:string, name:string, href:string) =
        a [
            _data ("on:click", $"@get('{href}')")
            _data ("class:btn-primary", $"$selectedNav === '{id}'")
            _data ("class:btn-disabled", $"$selectedNav === '{id}'")
            _class "btn btn-sm btn-soft"
            _children name
        ]
        
    let navbar =
        nav [
            _data ("signals:selectedNav", "")
            _class "navbar bg-base-100 shadow-sm flex items-center space-x-4"
            _children [
                navLink ("home", "Home", "/")
                navLink ("items", "Items", "/items")
            ]
        ]
    
    let layout (page:Element) =
        html [
            _lang "en"
            _data ("theme", "emerald")
            _children [
                head [
                    title "F# Datastar Example"
                    meta [_charset "utf-8"]
                    meta [_name "viewport"; _content "width=device-width, initial-scale=1"]
                    link [ _href "https://cdn.jsdelivr.net/npm/daisyui@5"; _rel "stylesheet"; _type "text/css" ]
                    link [ _href "https://cdn.jsdelivr.net/npm/daisyui@5/themes.css"; _rel "stylesheet"; _type "text/css" ]
                    script [ _src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ]
                    script [ _type "module"; _src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.6/bundles/datastar.js" ]
                    script [ _src "https://unpkg.com/echarts@6.0.0/dist/echarts.min.js" ]
                ]
                body [
                    _children [
                        navbar
                        div [
                            _class "max-w-4xl mx-auto py-8"
                            _children page
                        ]
                    ]
                ]
            ]
        ]
        
    let field (id:string, labelName:string, inputType:string) =
        label [
            _class "input"
            _children [
                span [
                    _class "label"
                    _children labelName
                ]
                input [
                    _type inputType
                    _data ("bind", id)
                    _name id
                ]
            ]
        ]
        
    let homePage =
        div [
            _id "page"
            _data ("init", "$selectedNav = 'home'; window.history.replaceState({}, '', '/')")
            _class "hero"
            _children [
                div [
                    _class "hero-content text-center"
                    _children [
                        div [
                            _class "max-w-md"
                            _children [
                                h1 [
                                    _class "text-5xl font-bold"
                                    _children "F# Datastar Example"
                                ]
                                p [
                                    _class "py-6"
                                    _children "Testing out Datastar with F# and Giraffe."
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        
    let createChartData (items:Item list) =
        items
        |> Seq.map (fun item -> [| item.x; item.y |])
        |> Seq.sort
        |> Seq.toArray
        
    let itemsChart (items:Item list) =
        let chartData = createChartData items
        let signals = {| _itemsChartData = chartData |}
        let signalsJs = Js.serialize signals
        div [
            _id "item-chart-container"
            _data ("signals", signalsJs)
            _data ("init", "renderChart($_itemsChartData)")
            _data ("on-signal-patch", "renderChart($_itemsChartData)")
            _data ("on-signal-patch-filter", "/^_itemsChartData$/")
            _children [
                div [
                    _id "item-chart"
                    _data ("on:chart-clicked", "$x = event.detail.x; $y = event.detail.y; @post('/items')")
                    _class "w-full h-96 mb-4"
                ]
                // language=javascript
                script """
                var chartDom = document.getElementById('item-chart');
                var myChart = echarts.init(chartDom, {renderer: 'svg'});
                myChart.getZr().on('click', function(params) {
                    const pointInPixel = [params.offsetX, params.offsetY];
                    const pointInGrid = myChart.convertFromPixel({ seriesIndex: 0 }, pointInPixel);
                    // check if click was inside plot area
                    const [x, y] = pointInGrid;
                    if (!isNaN(x) && !isNaN(y)) {
                        chartDom.dispatchEvent(new CustomEvent('chart-clicked', {
                            detail: { x: Math.round(x), y: Math.round(y) }
                        }));
                    }
                });

                function renderChart(data) {
                    var options = {
                        xAxis: {
                            type: 'value',
                            name: 'X Value',
                            minInterval: 1
                        },
                        yAxis: {
                            type: 'value',
                            name: 'Y Value',
                            minInterval: 1
                        },
                        series: [{
                            type: 'scatter',
                            data: data
                        }]
                    };
                    myChart.setOption(options);
                }
                """
                pre [
                    _data "json-signals"
                    _class "bg-gray-100 p-4 rounded-md overflow-x-auto max-h-64"
                ]
            ]
        ]
        
    let itemsPage (items:Item list) =
        div [
            _id "page"
            _data ("init", "$selectedNav = 'items'; window.history.replaceState({}, '', '/items')")
            _data ("on:item-created__window", "@get('/items/data')")
            _children [
                h1 [
                    _class "text-center text-3xl font-bold mb-4"
                    _children "Items"
                ]
                form [
                    _id "create-item-form"
                    _data ("on:submit", "@post('/items')")
                    _class "w-full border border-gray-200 p-4 rounded-md bg-white"
                    _children [
                        p [ _class "mb-4 font-bold"; _children "Create Item" ]
                        div [
                            _class "mb-4 grid grid-cols-1 gap-4 sm:grid-cols-3 "
                            _children [
                                field ("x", "x value", "number")
                                field ("y", "y value", "number")
                            ]
                        ]
                        div [
                            _class "w-full flex gap-4"
                            _children [
                                button [
                                    _type "submit"
                                    _class "btn btn-sm btn-primary"
                                    _children "Create"
                                ]
                                button [
                                    _type "button"
                                    _class "btn btn-sm btn-secondary"
                                    _data ("on:click", "$x = Math.floor(Math.random() * 11); $y = Math.floor(Math.random() * 11); @post('/items')")
                                    _children "Random"
                                ]
                            ]
                        ]
                    ]
                ]
                itemsChart items
            ]
        ]


module Handler =
    open FSharp.ViewEngine
    
    let renderView (view:Element) : HttpHandler =
        fun next ctx -> task {
            let html = Element.render view
            return! htmlString html next ctx
        }
        
    let javascriptString (script:string) : HttpHandler =
        setContentType "text/javascript"
        >=> setBodyFromString script
        
    let dispatch (name:string) : HttpHandler =
        fun next ctx -> task {
            // language=javascript
            let js = $"window.dispatchEvent(new CustomEvent('{ name }'))"
            return! javascriptString js next ctx
        }
    
    let renderPage (page:Element) : HttpHandler =
        fun next ctx -> task {
            match ctx.TryGetRequestHeader("Datastar-Request") with
            | Some "true" ->
                return! renderView page next ctx
            | _ ->
                let html = View.layout page |> Element.render
                return! htmlString html next ctx
        }
        
    let createItem : HttpHandler =
        fun next ctx -> task {
            let! signals = ctx.BindJsonAsync<{| x:int; y:int |}>()
            match signals.x, signals.y with
            | x, y when x >= 0 && y >= 0 ->
                Log.Information("👉 Creating item at ({x}, {y})", x, y)
                let item:Item = { x = signals.x; y = signals.y }
                itemStore.TryAdd(item, true) |> ignore
                return! dispatch "item-created" next ctx
            | _ ->
                Log.Warning("⚠️ Invalid item coordinates ({x}, {y})", signals.x, signals.y)
                return! Successful.NO_CONTENT next ctx
        }
        
    let getItemsPage : HttpHandler =
        fun next ctx -> task {
            let items = itemStore.Keys |> Seq.toList
            let page = View.itemsPage items
            return! renderPage page next ctx
        }
        
    let getItemsData : HttpHandler =
        fun next ctx -> task {
            let items = itemStore.Keys |> Seq.toList
            let data = View.createChartData items
            let signals = {| _itemsChartData = data |}
            return! json signals next ctx
        }
        
    let itemsApp : HttpHandler =
        choose [
            POST >=> createItem
            GET >=> choose [
                routex "(/?)" >=> getItemsPage
                route "/data" >=> getItemsData
            ]
        ]
        
    let app:HttpHandler =
        choose [
            route "/" >=> renderPage View.homePage
            subRoute "/items" itemsApp
        ]

let configureApp (app: WebApplication) =
    app.UseGiraffe(Handler.app)
        
let configureServices (services: IServiceCollection) =
    services
        .AddSerilog()
        .AddGiraffe() |> ignore

[<EntryPoint>]
let main args =
    Log.Logger <-
        LoggerConfiguration()
            .WriteTo.Console()
            .CreateLogger()
    try
        try
            let builder = WebApplication.CreateBuilder(args)
            configureServices builder.Services
            let app = builder.Build()
            configureApp app
            Log.Information("🚀 Launching app...")
            app.Run("https://0.0.0.0:5000")
            0 // Exit code
        with ex ->
            Log.Fatal(ex, "Application terminated unexpectedly")
            1 // Exit code
    finally
        Log.CloseAndFlush()
