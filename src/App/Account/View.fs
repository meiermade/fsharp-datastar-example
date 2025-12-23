module App.Account.View

open App.Infrastructure.AdaptiveOperators
open App.Common.View
open App.Account.Model
open FSharp.Data.Adaptive
open FSharp.ViewEngine
open type Html

let accountPage (model:Model.State) =
    let observations = model.observations
    let dates = observations |> Map.keys
    Page.primary(
        attrs=[
            _data ("on:paycheck-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/paycheck`)")
            _data ("on:rent-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/rent`)")
        ],
        content=[
            h1 [
                _class "text-center text-3xl font-bold mb-4"
                _children "Account"
            ]
            div [
                _class "flex items-center"
                _children [
                    div [
                        _class "p-4 flex flex-col"
                        _children [
                            span [
                                _class "mb-1 text-sm font-medium"
                                _children "Checking APY:"
                            ]
                            input [
                                _id "checking-apy"
                                _class "input input-sm"
                                _type "number"
                                _data ("on:change", $$"""@post('/account/checking-apy')""")
                                _data ("bind", "checkingApy")
                                _value (string !!model.inputs.checkingApy)
                                _step 0.01
                                _min 0
                                _max 1
                            ]
                        ]
                    ]
                    div [
                        _class "p-4 flex flex-col"
                        _children [
                            span [
                                _class "mb-1 text-sm font-medium"
                                _children "Savings APY:"
                            ]
                            input [
                                _id "savings-apy"
                                _class "input input-sm"
                                _type "number"
                                _data ("on:change", $$"""@post('/account/savings-apy')""")
                                _data ("bind", "savingsApy")
                                _value (string !!model.inputs.savingsApy)
                                _step 0.01
                                _min 0
                                _max 1
                            ]
                        ]
                    ]
                ]
            ]
            div [
                _class "overflow-x-auto"
                _children [
                    table [
                        _class "table table-sm table-pin-rows table-pin-columns"
                        _children [
                            thead [
                                tr [
                                    th [
                                        _class "flex flex-col bg-white z-10"
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
                                                _class "bg-white z-10"
                                                _children "Paycheck"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let amount' = model.inputs.paycheck[date]
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input input-sm"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('paycheck-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string !!amount')
                                                            _min 0
                                                            _step 50
                                                            _max 1_000_000
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10"
                                                _children "Rent"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let amount' = model.inputs.rent[date]
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input input-sm"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('rent-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string !!amount')
                                                            _min 0
                                                            _step 50
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10 whitespace-nowrap"
                                                _children "Checking Yield Balance"
                                            ]
                                            for date in dates do
                                                let balance =
                                                    observations[date].accounts
                                                    |> Map.tryFind Model.checkingYieldAccountId
                                                    |> Option.map AVal.force
                                                    |> Option.defaultValue 0m
                                                td [
                                                    _children [
                                                        span [
                                                            _class "input input-sm bg-gray-100"
                                                            _children (balance.ToString("C"))
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10 whitespace-nowrap"
                                                _children "Savings Yield Balance"
                                            ]
                                            for date in dates do
                                                let balance =
                                                    observations[date].accounts
                                                    |> Map.tryFind Model.savingsYieldAccountId
                                                    |> Option.map AVal.force
                                                    |> Option.defaultValue 0m
                                                td [
                                                    _children [
                                                        span [
                                                            _class "input input-sm bg-gray-100"
                                                            _children (balance.ToString("C"))
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10"
                                                _children "Checking Balance"
                                            ]
                                            for date in dates do
                                                let balance =
                                                    observations[date].accounts
                                                    |> Map.tryFind Model.checkingAccountId
                                                    |> Option.map AVal.force
                                                    |> Option.defaultValue 0m
                                                td [
                                                    _children [
                                                        span [
                                                            _class "input input-sm bg-gray-100"
                                                            _children (balance.ToString("C"))
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10"
                                                _children "Savings Balance"
                                            ]
                                            for date in dates do
                                                let balance =
                                                    observations[date].accounts
                                                    |> Map.tryFind Model.savingsAccountId
                                                    |> Option.map AVal.force
                                                    |> Option.defaultValue 0m
                                                td [
                                                    _children [
                                                        span [
                                                            _class "input input-sm bg-gray-100"
                                                            _children (balance.ToString("C"))
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
    
