module App.Index.View

open App.Common.View
open FSharp.ViewEngine
open type Html

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
    
