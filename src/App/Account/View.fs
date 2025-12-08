module App.Account.View

open App
open App.Common.View
open App.Account.Model
open FSharp.ViewEngine
open type Html

let accountPage (model:Model) =
    let dates = DateOnly.range model.startDate model.endDate
    let observations = model.observations
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
                                        _class "flex flex-col sticky left-0 bg-white z-10"
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
                                                _class "sticky left-0 bg-white z-10"
                                                _children "Deposit"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let deposit = !!model.inputs.deposits[date]
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('deposit-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string deposit)
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
                                                _class "sticky left-0 bg-white z-10"
                                                _children "Withdrawal"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let withdrawal = !!model.inputs.withdrawals[date]
                                                let balance = !!observations[date].balance
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('withdrawal-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string withdrawal)
                                                            _min 0
                                                            _step 50
                                                            _max (float balance)
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "sticky left-0 bg-white z-10 border-t-2 border-gray-300 no-wrap"
                                                _children "Accrued Yield"
                                            ]
                                            for date in dates do
                                                let yieldAccrued = !!observations[date].yieldAccrued
                                                td [
                                                    _class "border-t-2 border-gray-300"
                                                    _children [
                                                        span [
                                                            _class "input bg-gray-100"
                                                            _children (yieldAccrued.ToString("C"))
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "sticky left-0 bg-white z-10"
                                                _children "Balance"
                                            ]
                                            for date in dates do
                                                let balance = !!observations[date].balance
                                                td [
                                                    _children [
                                                        span [
                                                            _class "input bg-gray-100"
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
    
