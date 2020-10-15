namespace Craft.Engine

open System.Diagnostics.Tracing

module CoreFX =
    
    type GCListener() =
        inherit EventListener()
    
        let mutable timeStart = 0L 
        
        override x.OnEventSourceCreated(es: EventSource) =
            if es.Name = "Microsoft-Windows-DotNETRuntime" then
                x.EnableEvents(es, EventLevel.Informational, LanguagePrimitives.EnumOfValue 1L)
                
        override x.OnEventWritten(args: EventWrittenEventArgs) =
            if args.EventName.Contains "GCStart" then
                timeStart <- args.TimeStamp.Ticks
            elif args.EventName.Contains "GCEnd" then
                let timeEnd = args.TimeStamp.Ticks
                let time = (float(timeEnd - timeStart)*0.0001)
                if time >= 3.0 then 
                    let index = System.Int64.TryParse(args.Payload.[0].ToString()) |> snd  
                    printfn "GC#%i took %.3f ms" index time 

