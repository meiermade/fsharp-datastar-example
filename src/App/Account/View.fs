module App.Account.View

open App.Infrastructure.AdaptiveOperators
open App.Common.View
open App.Account.Model
open FSharp.ViewEngine
open type Html

let accountPage (model:Model.State) =
    let observations = model.observations
    let dates = observations |> Map.keys
    Page.primary(
        attrs=[
            _data ("on:checking-deposit-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/checking-deposit`)")
            _data ("on:savings-transfer-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/savings-transfer`)")
            _data ("on:savings-withdrawal-changed", "$amount = evt.detail.amount; $date = evt.detail.date; @post(`/account/savings-withdrawal`)")
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
                                                _children "Checking Deposit"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let amount' = model.inputs.checkingDeposits[date]
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input input-sm"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('checking-deposit-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
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
                                                _children "Savings Transfer"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let balance' = observations[date].accounts[Model.checkingAccountId].balance
                                                let amount' = model.inputs.savingsTransfers[date]
                                                let originalBalance' = balance' +. amount'
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input input-sm"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('savings-transfer-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string !!amount')
                                                            _min 0
                                                            _step 50
                                                            _max (float !!originalBalance')
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10"
                                                _children "Savings Withdrawal"
                                            ]
                                            for date in dates do
                                                let dateStr = date.ToString("yyyy-MM-dd")
                                                let balance' = observations[date].accounts[Model.savingsAccountId].balance
                                                let amount' = model.inputs.savingsWithdrawals[date]
                                                let originalBalance' = balance' +. amount'
                                                td [
                                                    _children [
                                                        input [
                                                            _class "input input-sm"
                                                            _type "number"
                                                            _data ("on:change", $$"""el.dispatchEvent(new CustomEvent('savings-withdrawal-changed', { detail: { date: '{{ dateStr }}', amount: Number(el.value) }, bubbles: true }))""")
                                                            _value (string !!amount')
                                                            _min 0
                                                            _step 50
                                                            _max (float !!originalBalance')
                                                        ]
                                                    ]
                                                ]
                                        ]
                                    ]
                                    tr [
                                        _children [
                                            th [
                                                _class "bg-white z-10 border-t-2 border-gray-300 whitespace-nowrap"
                                                _children "Checking Yield Balance"
                                            ]
                                            for date in dates do
                                                let balance = !!observations[date].accounts[Model.checkingYieldAccountId].balance
                                                td [
                                                    _class "border-t-2 border-gray-300"
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
                                                let balance = !!observations[date].accounts[Model.savingsYieldAccountId].balance
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
                                                let balance = !!observations[date].accounts[Model.checkingAccountId].balance
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
                                                let balance = !!observations[date].accounts[Model.savingsAccountId].balance
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
    
