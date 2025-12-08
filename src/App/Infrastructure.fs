[<AutoOpen>]
module App.Infrastructure

open Giraffe
open Microsoft.AspNetCore.Http
open System
open System.Text.Json

module Json =
    let serialize (value:'T) = JsonSerializer.Serialize(value)
    
module Js =
    let serialize (value:'T) = JsonSerializer.Serialize(value).Replace("\"", "'")
    
module Guid =
    let create () = Guid.CreateVersion7()
    
module Option =
    let fail msg = function
        | Some v -> v
        | None -> failwith msg
    
module DateOnly =
    let daysBetween (startDate:DateOnly) (endDate:DateOnly) =
        endDate.DayNumber - startDate.DayNumber
    let range (startDate:DateOnly) (endDate:DateOnly) =
        Seq.initInfinite startDate.AddDays
        |> Seq.takeWhile (fun d -> d <= endDate)
        |> Seq.toList
    
type HttpContext with
    member this.IsDatastar =
        match this.TryGetRequestHeader("Datastar-Request") with
        | Some "true" -> true
        | _ -> false
