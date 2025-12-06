open FSharp.Data.Adaptive
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Serilog
open System
open System.Text.Json
open System.Threading.Tasks
open System.Collections.Concurrent
open StarFederation.Datastar.DependencyInjection

module Js =
    let serialize (value:'T) = JsonSerializer.Serialize(value).Replace("\"", "'")
    
module Guid =
    let create () = Guid.CreateVersion7()
    
module DateOnly =
    let daysBetween (startDate:DateOnly) (endDate:DateOnly) =
        endDate.DayNumber - startDate.DayNumber
    let range (startDate:DateOnly) (endDate:DateOnly) =
        Seq.initInfinite startDate.AddDays
        |> Seq.takeWhile (fun d -> d <= endDate)
        |> Seq.toList
    
type Signals =
    { selectedNav: string }

type Item =
    { id:Guid
      x:int
      y:int }
        
let itemStore = ConcurrentDictionary<int*int,Guid>()
for x in 1 .. 3 do
    let guid = Guid.create()
    itemStore.TryAdd((x, x), guid) |> ignore
    
[<AutoOpen>]
module AdaptiveOperators =
    let ( +. ) a b = AVal.map2 (+) a b
    let ( -. ) a b = AVal.map2 (-) a b
    let ( *. ) a b = AVal.map2 (*) a b
    let ( /. ) a b = AVal.map2 (/) a b
    let (!) a = AVal.constant a
    let (!~) a = cval a
    let (!!) a = AVal.force a
        
type AccountInputs =
    { deposits:Map<DateOnly,cval<decimal>>
      withdrawals:Map<DateOnly,cval<decimal>>
      apy:cval<decimal> }
    
type Account =
    { apy:aval<decimal>
      yieldAccruedDate:DateOnly
      yieldAccrued:aval<decimal>
      balance:aval<decimal> }
    
type AccountEvent =
    | AmountDeposited of {| date:DateOnly; amount: aval<decimal> |}
    | AmountWithdrawn of {| date:DateOnly; amount: aval<decimal> |}
    | YieldAccrued of {| date:DateOnly; amount: aval<decimal> |}
    | YieldReceived of {| date:DateOnly; amount: aval<decimal> |}
    | Snapshotted of {| date:DateOnly; account:Account |}
module AccountEvent =
    let toString (event:AccountEvent) =
        match event with
        | AmountDeposited e -> $"AmountDeposited(date={e.date}, amount={!!e.amount})"
        | AmountWithdrawn e -> $"AmountWithdrawn(date={e.date}, amount={!!e.amount})"
        | YieldAccrued e -> $"YieldAccrued(date={e.date}, amount={!!e.amount})"
        | YieldReceived e -> $"YieldReceived(date={e.date}, amount={!!e.amount})"
        | Snapshotted e -> $"Snapshotted(date={e.date}, balance={!!e.account.balance})"
        
type AccountCommand =
    | StartDay of DateOnly
    | Deposit of DateOnly * aval<decimal>
    | Withdraw of DateOnly * aval<decimal>
    | Snapshot of DateOnly
    
module Account =
    let initial startDate apy : Account =
        { apy = apy
          yieldAccruedDate = startDate
          yieldAccrued = AVal.constant 0m
          balance = AVal.constant 0m }
    let evolve state event : Account =
        match event with
        | AmountDeposited e ->
            { state with balance = state.balance +. e.amount }
        | AmountWithdrawn e ->
            { state with balance = state.balance -. e.amount }
        | YieldAccrued e ->
            { state with
                yieldAccruedDate = e.date
                yieldAccrued = state.yieldAccrued +. e.amount }
        | YieldReceived e ->
            { state with balance = state.balance +. e.amount }
        | Snapshotted _ -> state
        
    let fold:Account -> AccountEvent seq -> Account = Seq.fold evolve
    
    let decide command (account:Account) : AccountEvent list =
        match command with
        | StartDay date ->
            let days = DateOnly.daysBetween account.yieldAccruedDate date
            let dpy = account.apy /. !365m
            let yieldAccrued = account.balance *. dpy *. !(decimal days)
            [
                YieldAccrued {| date = date; amount = yieldAccrued |}
                if date.DayOfWeek = DayOfWeek.Friday then
                    let yieldReceived = account.yieldAccrued +. yieldAccrued |> AVal.map (fun v -> Math.Round(v, 2))
                    YieldReceived {| date = date; amount = yieldReceived |}
            ]
        | Deposit (date, amount) ->
            [ AmountDeposited {| date = date; amount = amount |} ]
        | Withdraw (date, amount) ->
            let amount = AVal.map2 min account.balance amount
            [ AmountWithdrawn {| date = date; amount = amount |} ]
        | Snapshot date ->
            [ Snapshotted {| date = date; account = account |} ]
        
type Model =
    { startDate: DateOnly
      endDate: DateOnly
      inputs: AccountInputs
      observedDate: DateOnly
      account: Account
      events: AccountEvent list }
        
module Model =
    let initial (startDate:DateOnly, endDate:DateOnly, inputs:AccountInputs) : Model =
        let account = Account.initial startDate inputs.apy
        let events = [ Snapshotted {| date = startDate; account = account |} ]
        { startDate = startDate
          endDate = endDate
          inputs = inputs
          observedDate = startDate
          account = account
          events = events }
    let evolve (model:Model) (date:DateOnly) : Model =
        let inputs = model.inputs
        let account = model.account
        let deposit = inputs.deposits[date]
        let withdrawal = inputs.withdrawals[date]
        let commands =
            [ StartDay date
              Deposit (date, deposit)
              Withdraw (date, withdrawal)
              Snapshot date ]
        let newEvents, newAccount =
            (([], account), commands)
            ||> Seq.fold (fun (e, a) c ->
                let newEvents = Account.decide c a
                let newAccount = Account.fold a newEvents
                e @ newEvents, newAccount)
        { model with
            observedDate = date
            events = model.events @ newEvents
            account = newAccount }
    let build startDate endDate inputs : Model =
        let initialModel = initial (startDate, endDate, inputs)
        let dates = DateOnly.range startDate endDate
        (initialModel, dates) ||> Seq.fold evolve
        
let startDate = DateOnly(2025, 1, 1)
let endDate = DateOnly(2025, 1, 31)
let dates = DateOnly.range startDate endDate
let deposits = dates |> List.map (fun d -> (d, cval 100m)) |> Map.ofList
let withdrawals = dates |> List.map (fun d -> (d, cval 50m)) |> Map.ofList
let accountInputs:AccountInputs =
    { deposits = deposits
      withdrawals = withdrawals
      apy = cval 0.05m }
let model = Model.build startDate endDate accountInputs

module View =
    open FSharp.ViewEngine
    open type Html
    
    type Page =
        static member primary(content:Element seq, ?attrs:Attribute seq) =
            let attrs = defaultArg attrs Seq.empty
            div [
                _id "page"
                _children content
                yield! attrs
            ]
    
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
                navLink ("account", "Account", "/account")
                navLink ("time", "Time", "/time")
            ]
        ]
    
    let layout (page:Element, signals:Signals) =
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
                    _data ("signals", Js.serialize signals)
                    _children [
                        navbar
                        div [
                            _class "max-w-7xl mx-auto py-8"
                            _children page
                        ]
                    ]
                ]
            ]
        ]
        
    let numberField (id:string, labelName:string, min:int, max:int) =
        label [
            _class "input"
            _children [
                span [
                    _class "label"
                    _children labelName
                ]
                input [
                    _type "number"
                    _data ("bind", id)
                    _name id
                    _min min
                    _max max
                ]
            ]
        ]
        
    let homePage =
        Page.primary(
            content=[
                div [
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
            ]
        )
        
    let createChartData (items:Item seq) =
        items
        |> Seq.sortBy _.id
        |> Seq.map (fun item -> [| item.x; item.y |])
        |> Seq.toArray
        
    let itemsChart (items:Item seq) =
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
                            minInterval: 1,
                            min: 0,
                            max: 10
                        },
                        yAxis: {
                            type: 'value',
                            name: 'Y Value',
                            minInterval: 1,
                            min: 0,
                            max: 10
                        },
                        tooltip: {
                            trigger: 'item', // show on point hover
                            formatter: (params) => {
                                const [x, y] = params.value;
                                return `(${x},${y})`;
                            }
                        },
                        series: [{
                            type: 'scatter',
                            data: data,
                            universalTransition: false,
                            animationDurationUpdate: (idx) => {
                                return idx === data.length - 1 ? 500 : 0;
                            },
                            animationDelayUpdate: (idx) => {
                                return idx === data.length - 1 ? 0 : 0;
                            }
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
        
    let itemsPage (items:Item seq) =
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
                                numberField ("x", "x value", 0, 10)
                                numberField ("y", "y value", 0, 10)
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
        
    let accountPage (model:Model) =
        Log.Information("👉 APY: {apy}", !!model.inputs.apy)
        let dates = DateOnly.range model.startDate model.endDate
        let snapshots =
            model.events
            |> List.choose (function
                | AccountEvent.Snapshotted e -> Some (e.date, e.account)
                | _ -> None)
            |> Map.ofList
        Page.primary(
            attrs=[
                _data ("on:deposit-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/deposit`)")
                _data ("on:withdrawal-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/withdrawal`)")
            ],
            content=[
                h1 [
                    _class "text-center text-3xl font-bold mb-4"
                    _children "Account"
                ]
                div [
                    _class "p-4 flex flex-col"
                    _children [
                        span [
                            _class "mb-1 text-sm font-medium"
                            _children "Annual Percentage Yield (APY):"
                        ]
                        span [
                            _class "mb-2 text-xs"
                            _children "Compounded on Fridays"
                        ]
                        input [
                            _class "input"
                            _type "number"
                            _data ("on:change", $$"""@post('/account/apy')""")
                            _data ("bind", "apy")
                            _value (string !!model.inputs.apy)
                            _step 0.01
                            _min 0
                            _max 1
                        ]
                    ]
                ]
                div [
                    _class "overflow-x-auto"
                    _children [
                        table [
                            _class "table table-sm"
                            _children [
                                thead [
                                    tr [
                                        th [
                                            _class "flex flex-col"
                                            _children [
                                                span [
                                                    _children "Date"
                                                ]
                                                span [
                                                    _class "text-xs text-gray-500"
                                                    _children "Day of Week"
                                                ]
                                            ]
                                        ]
                                        for date in dates do
                                            th [
                                                _children [
                                                    div [
                                                        _class "flex flex-col"
                                                        _children [
                                                            span [
                                                                _children (date.ToString("yyyy-MM-dd"))
                                                            ]
                                                            span [
                                                                _class "text-xs text-gray-500"
                                                                _children (date.DayOfWeek.ToString())
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                    ]
                                ]
                                tbody [
                                    _children [
                                        tr [
                                            _children [
                                                th [
                                                    _children "Deposits"
                                                ]
                                                for date in dates do
                                                    let dateStr = date.ToString("yyyy-MM-dd")
                                                    let deposit = model.inputs.deposits[date]
                                                    td [
                                                        _children [
                                                            input [
                                                                _class "input"
                                                                _type "number"
                                                                _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('deposit-changed', { detail: { date: '{{ dateStr }}', amount: el.value }, bubbles: true }))""")
                                                                _value (string deposit.Value)
                                                                _min 0
                                                                _step 100
                                                                _max 1_000_000
                                                            ]
                                                        ]
                                                    ]
                                            ]
                                        ]
                                        tr [
                                            _children [
                                                th [
                                                    _children "Withdrawals"
                                                ]
                                                for date in dates do
                                                    let dateStr = date.ToString("yyyy-MM-dd")
                                                    let withdrawal = model.inputs.withdrawals[date]
                                                    let balance = snapshots[date].balance
                                                    td [
                                                        _children [
                                                            input [
                                                                _class "input"
                                                                _type "number"
                                                                _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('withdrawal-changed', { detail: { date: '{{ dateStr }}', amount: el.value }, bubbles: true }))""")
                                                                _value (string withdrawal.Value)
                                                                _min 0
                                                                _step 50
                                                                _max (float !!balance)
                                                            ]
                                                        ]
                                                    ]
                                            ]
                                        ]
                                        tr [
                                            _children [
                                                th [
                                                    _class "border-t-2 border-gray-300"
                                                    _children "Balances"
                                                ]
                                                for date in dates do
                                                    let snapshot = snapshots[date]
                                                    td [
                                                        _class "border-t-2 border-gray-300"
                                                        _children [
                                                            span [
                                                                _class "input bg-gray-100"
                                                                _children (!!snapshot.balance |> _.ToString("C"))
                                                            ]
                                                        ]
                                                    ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        )
        
    let currentTimeElement (currentTime:DateTimeOffset) =
        p [
            _id "current-time"
            _class "text-center"
            _children (currentTime.ToString("yyyy-MM-dd HH:mm:ss"))
        ]
        
    let timePage (currentTime:DateTimeOffset) =
        Page.primary(
            attrs=[
                _data ("signals:controller", "new AbortController()")
                _data ("init", "@get('/time/update', { requestCancellation: $controller })")
            ],
            content=[
                h1 [
                    _class "text-center text-3xl font-bold mb-4"
                    _children "Current Time"
                ]
                div [
                    _class "flex justify-center space-x-4"
                    _children [
                        currentTimeElement currentTime
                        button [
                            _data ("on:click", "$controller = new AbortController(); @get('/time/update', { requestCancellation: $controller })")
                            _class "btn btn-sm btn-primary"
                            _children "Start"
                        ]
                        button [
                            _data ("on:click", "$controller.abort()")
                            _class "btn btn-sm btn-secondary"
                            _children "Stop"
                        ]
                    ]
                ]
            ]
        )

module Handler =
    open FSharp.ViewEngine
    
    type HttpContext with
        member this.IsDatastar =
            match this.TryGetRequestHeader("Datastar-Request") with
            | Some "true" -> true
            | _ -> false
            
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
        
    let itemsApp : HttpHandler =
        choose [
            POST >=> createItem
            GET >=> choose [
                routex "(/?)" >=> getItemsPage
                route "/data" >=> getItemsData
            ]
        ]
        
    let getAccountPage : HttpHandler =
        fun next ctx -> task {
            let page = View.accountPage model
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
            let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:string |}>()
            Log.Information("👉 Updating deposit {date} to {amount}", signals.date, signals.amount)
            transact(fun () -> model.inputs.deposits[signals.date].Value <- decimal signals.amount)
            return! getAccountPage next ctx
        }
        
    let updateWithdrawal : HttpHandler =
        fun next ctx -> task {
            if not ctx.IsDatastar then failwith "Not a Datastar request"
            let ds = ctx.GetService<IDatastarService>()
            let! signals = ds.ReadSignalsAsync<{| date:DateOnly; amount:string |}>()
            Log.Information("👉 Updating withdrawal {date} to {amount}", signals.date, signals.amount)
            transact(fun () -> model.inputs.withdrawals[signals.date].Value <- decimal signals.amount)
            return! getAccountPage next ctx
        }
        
    let updateYield : HttpHandler =
        fun next ctx -> task {
            if not ctx.IsDatastar then failwith "Not a Datastar request"
            let ds = ctx.GetService<IDatastarService>()
            let! signals = ds.ReadSignalsAsync<{| apy:decimal |}>()
            Log.Information("👉 Updating APY to {apy}", signals.apy)
            transact(fun () -> model.inputs.apy.Value <- signals.apy)
            return! getAccountPage next ctx
        }
        
    let accountApp : HttpHandler =
        choose [
            routex "(/?)" >=> GET >=> getAccountPage
            route "/apy" >=> POST >=> updateYield
            route "/deposit" >=> POST >=> updateDeposit
            route "/withdrawal" >=> POST >=> updateWithdrawal
        ]
        
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
        
    let timeApp : HttpHandler =
        choose [
            GET >=> choose [
                routex "(/?)" >=> getTimePage
                route "/update" >=> getUpdatedTime
            ]
        ]
        
    let app:HttpHandler =
        choose [
            route "/" >=> getHomePage
            subRoute "/items" itemsApp
            subRoute "/account" accountApp
            subRoute "/time" timeApp
        ]

let configureApp (app: WebApplication) =
    app.UseGiraffe(Handler.app)
        
let configureServices (services: IServiceCollection) =
    services
        .AddSerilog()
        .AddDatastar()
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
