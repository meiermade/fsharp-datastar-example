module App.Item.Model

open App
open System
open System.Collections.Concurrent

type Item =
    { id:Guid
      x:int
      y:int }
        
let itemStore = ConcurrentDictionary<int*int,Guid>()
for x in 1 .. 3 do
    let guid = Guid.create()
    itemStore.TryAdd((x, x), guid) |> ignore
    
