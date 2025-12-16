module App.Common.View

open App
open App.Common.Model
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
            navLink ("simple-account", "Simple Account", "/simple-account")
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
    
