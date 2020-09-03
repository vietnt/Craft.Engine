// Learn more about F# at http://fsharp.org

open Craft.Engine

[<EntryPoint>]
let main argv =
    
    let init d =
        let gpu = GPU.initGpu false |> Result.get
        
        ()
    
    0 // return an integer exit code
