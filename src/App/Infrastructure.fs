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
        
    let map (toSy:'Sx -> 'Sy) (decider:Decider<'C,'E,'S,'Sx>) : Decider<'C,'E,'S,'Sy> =
        { initialState = toSy decider.initialState
          evolve =
              fun s e ->
                  let sx = decider.evolve s e
                  toSy sx
          decide = decider.decide }
        
    let map2 (f:'Sx -> 'Sy -> 'Sz) (deciderX:Decider<'C,'E,'S,'Sx>) (deciderY:Decider<'C,'E,'S,'Sy>) : Decider<'C,'E,'S,'Sz> =
        { initialState =
            let sx = deciderX.initialState
            let sy = deciderY.initialState
            f sx sy
          evolve =
              fun s e ->
                  let sx = deciderX.evolve s e
                  let sy = deciderY.evolve s e
                  f sx sy
          decide =
              fun c s ->
                  deciderX.decide c s
                  @ deciderY.decide c s }
        
    let apply f x = map2 id f x
    
module DeciderOperators =
    let (<!>) = Decider.map
    let (<*> ) = Decider.apply
    
type Reactor<'C,'E,'Sx,'Sy> =
    { initialState:'Sy
      decide:'C -> 'Sx -> 'E list
      evolve:'Sx -> 'E -> 'Sy
      resume:'Sx -> 'C list
      react:'Sx -> 'E -> 'C list }
    
type Reactor<'C,'E,'S> = Reactor<'C,'E,'S,'S>
    
module Reactor =
    let create initialState evolve decide resume react =
        { initialState = initialState
          evolve = evolve
          decide = decide
          resume = resume
          react = react }
        
    let adaptDecider tryToCx tryToEx toEy toSx (decider:Decider<'Cx,'Ex,'Sx>) : Reactor<'Cy,'Ey,'Sy,'Sx> =
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
                  | None -> List.empty
          resume = fun _sy -> List.empty
          react = fun _sy _ey -> List.empty }
        
    let adapt toCy tryToCx tryToEx toEy toSx (reactor:Reactor<'Cx,'Ex,'Sx>) : Reactor<'Cy,'Ey,'Sy,'Sx> =
        { initialState = reactor.initialState
          evolve =
              fun sy ey ->
                  match tryToEx ey with
                  | Some ex -> reactor.evolve (toSx sy) ex
                  | None -> toSx sy
          decide =
              fun cy sy ->
                  match tryToCx cy with
                  | Some cx -> reactor.decide cx (toSx sy) |> List.map toEy
                  | None -> List.empty
          resume =
              fun sy ->
                  reactor.resume (toSx sy)
                  |> List.map toCy
          react =
              fun sy ey ->
                  match tryToEx ey with
                  | Some ex ->
                      let cxl = reactor.react (toSx sy) ex
                      cxl |> List.map toCy
                  | None -> List.empty }
        
    let map (toSy:'Sx -> 'Sy) (reactor:Reactor<'C,'E,'S,'Sx>) : Reactor<'C,'E,'S,'Sy> =
        { initialState = toSy reactor.initialState
          evolve =
              fun s e ->
                  let sx = reactor.evolve s e
                  toSy sx
          decide = reactor.decide
          resume = reactor.resume
          react = reactor.react }
        
    let map2 (f:'Sx -> 'Sy -> 'Sz) (reactorX:Reactor<'C,'E,'S,'Sx>) (reactorY:Reactor<'C,'E,'S,'Sy>) : Reactor<'C,'E,'S,'Sz> =
        { initialState =
            let sx = reactorX.initialState
            let sy = reactorY.initialState
            f sx sy
          evolve =
              fun s e ->
                  let sx = reactorX.evolve s e
                  let sy = reactorY.evolve s e
                  f sx sy
          decide =
              fun c s ->
                  reactorX.decide c s
                  @ reactorY.decide c s
          resume =
              fun s ->
                  let clx = reactorX.resume s
                  let cly = reactorY.resume s
                  clx @ cly
          react =
              fun s e ->
                  let clx = reactorX.react s e
                  let cly = reactorY.react s e
                  clx @ cly }
        
    let apply f x = map2 id f x
    
    let run (commands:'C list) (reactor:Reactor<'C,'E,'S>) =
        let rec fold s cl el =
            match el with
            | [] -> s, cl
            | e::rel ->
                let ns = reactor.evolve s e
                let ncl = reactor.react s e
                fold ns (cl @ ncl) rel
                
        let rec loop s cl =
            match cl with
            | [] ->
                match reactor.resume s with
                | [] -> s
                | cl -> loop s cl
            | c::rcl ->
                let el = reactor.decide c s
                let ns, ncl = fold s rcl el
                loop ns ncl

        loop reactor.initialState commands
    
module ReactorOperators =
    let (<!>) = Reactor.map
    let (<*> ) = Reactor.apply
    
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
