[<AutoOpen>]
module App.Infrastructure

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
        
type Evolver<'E,'Sx,'Sy> =
    { initialState:'Sy
      evolve:'Sx -> 'E -> 'Sy }
    
type Evolver<'E,'S> = Evolver<'E,'S,'S>
    
module Evolver =
    let create initialState evolve =
        { initialState = initialState
          evolve = evolve }
        
    let many (evolvers:Map<'K,Evolver<'E,'S>>) : Evolver<'K * 'E,Map<'K,'S>> =
        let evolve states (k, e) =
            let evolver = evolvers |> Map.tryFind k |> Option.fail $"No evolver for key '{k}'."
            states
            |> Map.change k (function
                | Some s -> Some (evolver.evolve s e)
                | None -> Some (evolver.evolve evolver.initialState e))
        { initialState = evolvers |> Map.map (fun _ evolver -> evolver.initialState)
          evolve = evolve }
        
    let withInitialState initialState (evolver:Evolver<'E,'S>) =
        { evolver with initialState = initialState }
        
    let adapt tryToEx toSx (evolver:Evolver<'Ex,'Sx>) : Evolver<'Ey,'Sy,'Sx> =
        { initialState = evolver.initialState
          evolve =
              fun sy ey ->
                  match tryToEx ey with
                  | Some ex -> evolver.evolve (toSx sy) ex
                  | None -> toSx sy }
        
    let map (f:'Sa -> 'Sb) (evolver:Evolver<_,'S,'Sa>) : Evolver<_,'S,'Sb> =
        { initialState = f evolver.initialState
          evolve = fun s e -> evolver.evolve s e |> f }
        
    let map2 (f:'Sa -> 'Sb -> 'Sc) (evolverA:Evolver<_,'S,'Sa>) (evolverB:Evolver<_,'S,'Sb>) : Evolver<_,'S,'Sc> =
        { initialState = f evolverA.initialState evolverB.initialState
          evolve =
              fun s e ->
                  let newA = evolverA.evolve s e
                  let newB = evolverB.evolve s e
                  f newA newB }
        
    let apply f x = map2 id f x
    
module EvolverOperators =
    let (<!>) = Evolver.map
    let (<*> ) = Evolver.apply
    
type Decider<'C,'E,'Sx,'Sy> =
    { initialState:'Sy
      evolve:'Sx -> 'E -> 'Sy
      decide:'C -> 'Sx -> 'E list }
    
type Decider<'C,'E,'S> = Decider<'C,'E,'S,'S>
    
module Decider =
    let create initialState evolve decide =
        { initialState = initialState
          evolve = evolve
          decide = decide }
        
    let withInitialState initialState (decider:Decider<'C,'E,'S>) =
        { decider with initialState = initialState }
        
    let many (decider:Decider<'C,'E,'S>) : Decider<'K * 'C,'K * 'E,Map<'K,'S>> =
        let evolve states (k, e) =
            states
            |> Map.change k (function
                | Some s -> Some (decider.evolve s e)
                | None -> Some (decider.evolve decider.initialState e))
        let decide (k, c) states =
            states
            |> Map.tryFind k
            |> Option.map (fun s -> decider.decide c s |> List.map (fun e -> k, e))
            |> Option.defaultValue List.empty
        { initialState = Map.empty
          evolve = evolve
          decide = decide }
        
    let adaptEvolver tryToEx toSx (evolver:Evolver<'Ex,'Sx>) : Decider<'Cy,'Ey,'Sy,'Sx> =
        { initialState = evolver.initialState
          evolve =
              fun sy ey ->
                  match tryToEx ey with
                  | Some ex -> evolver.evolve (toSx sy) ex
                  | None -> toSx sy
          decide = fun _cy _sy -> List.empty }
        
    let adapt tryToCx tryToEx toEy toSx (decider:Decider<'Cx,'Ex,'Sx>) : Decider<'Cy,'Ey,'Sy,'Sx> =
        { initialState = decider.initialState
          evolve =
              fun sy ey ->
                  match tryToEx ey with
                  | Some ex -> decider.evolve (toSx sy) ex
                  | None -> toSx sy
          decide =
              fun cy sy ->
                  match tryToCx cy with
                  | Some cx -> decider.decide cx (toSx sy) |> List.map toEy
                  | None -> List.empty }
        
    let map (f:'Sa -> 'Sb) (decider:Decider<_,_,'S,'Sa>) : Decider<_,_,'S,'Sb> =
        { initialState = f decider.initialState
          evolve = fun s e -> decider.evolve s e |> f
          decide = decider.decide }
        
    let map2 (f:'Sa -> 'Sb -> 'Sc) (deciderA:Decider<_,_,'S,'Sa>) (deciderB:Decider<_,_,'S,'Sb>) : Decider<_,_,'S,'Sc> =
        { initialState = f deciderA.initialState deciderB.initialState
          evolve =
              fun s e ->
                  let newA = deciderA.evolve s e
                  let newB = deciderB.evolve s e
                  f newA newB
          decide =
              fun c s ->
                  deciderA.decide c s
                  @ deciderB.decide c s }
        
    let apply f x = map2 id f x

module DeciderOperators =
    let (<!>) = Decider.map
    let (<*> ) = Decider.apply
    
module AdaptiveOperators =
    open FSharp.Data.Adaptive
    
    let inline ( +. ) a b = AVal.map2 (+) a b
    let inline ( -. ) a b = AVal.map2 (-) a b
    let inline ( *. ) a b = AVal.map2 (*) a b
    let inline ( /. ) a b = AVal.map2 (/) a b
    let inline (!) a = AVal.constant a
    let inline (!~) a = cval a
    let inline (!!) a = AVal.force a
    
[<AutoOpen>]
module HttpContextExtensions =
    open Microsoft.AspNetCore.Http
    open Giraffe
    
    type HttpContext with
        member this.IsDatastar =
            match this.TryGetRequestHeader("Datastar-Request") with
            | Some "true" -> true
            | _ -> false
