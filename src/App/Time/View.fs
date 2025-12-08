module App.Time.View

open App.Common.View
open FSharp.ViewEngine
open System
open type Html

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

