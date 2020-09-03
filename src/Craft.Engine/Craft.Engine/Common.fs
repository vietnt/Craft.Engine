[<AutoOpen>]
module Craft.Engine.Common

type Result<'a> =
    | Success of 'a
    | Failure of string
    
module Result =
    
    let get (r: Result<'a>) =
        match r with
        | Success v -> v
        | Failure s -> failwith s