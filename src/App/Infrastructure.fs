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
        
type Decider<'E,'Si,'So> =
    { state:'So
      evolve: 'Si -> 'E -> 'So
      decide: 'Si -> 'E list }
    
type Decider<'E,'S> = Decider<'E,'S,'S>
    
module Decider =
    let create initialState evolve decide =
        { state = initialState
          evolve = evolve
          decide = decide }
        
    let adapt tryToEi toEo toSi (decider:Decider<'Ei,'Si,'Si>) : Decider<'Eo,'So,'Si> =
        { state = decider.state
          evolve =
              fun so eo ->
                  match tryToEi eo with
                  | Some ei -> decider.evolve (toSi so) ei
                  | None -> toSi so
          decide =
              fun so ->
                  decider.decide (toSi so) |> List.map toEo }
        
    let map (f:'Sa -> 'Sb) (decider:Decider<_,'Si,'Sa>) : Decider<_,'Si,'Sb> =
        { state = f decider.state
          evolve = fun si e -> decider.evolve si e |> f
          decide = decider.decide }
        
    let map2 (f:'Sa -> 'Sb -> 'Sc) (deciderA:Decider<_,'Si,'Sa>) (deciderB:Decider<_,'Si,'Sb>) : Decider<_,'Si,'Sc> =
        { state = f deciderA.state deciderB.state
          evolve =
              fun si e ->
                  let newA = deciderA.evolve si e
                  let newB = deciderB.evolve si e
                  f newA newB
          decide =
              fun si ->
                  deciderA.decide si
                  @ deciderB.decide si }
        
    let apply f x = map2 id f x
    
    let rec run (decider:Decider<'E,'S>) : Decider<'E,'S> =
        match decider.decide decider.state with
        | [] -> decider
        | events ->
            let newState = Seq.fold decider.evolve decider.state events
            let newDecider = { decider with state = newState }
            run newDecider
            
    let state (decider:Decider<_,_>) = decider.state
    
module DeciderOperators =
    let (<!>) = Decider.map
    let (<*> ) = Decider.apply
    
type HttpContext with
    member this.IsDatastar =
        match this.TryGetRequestHeader("Datastar-Request") with
        | Some "true" -> true
        | _ -> false
