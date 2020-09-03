module Craft.Engine.Game

open System
open System.Diagnostics
open System.Threading

type ScreenMode =
    | FullScreen
    | Windows
    | BorderLess

type AppModel = {        
    desc               : OS.WindowsDesc         
}

type InputState = {
    mouseLeftDown     : bool
    mouseLeftUp       : bool
    mouseX            : int
    mouseY            : int
    keys              : Map<int,bool>
}
    
let create name width height mode =
    OS.SetProcessDPIAware() |> ignore        
    let desc : OS.WindowsDesc = {
        windowedRect = OS.newRect 0 0 width height
        fullscreenRect = Unchecked.defaultof<_>
        clientRect = Unchecked.defaultof<_>
        bigIcon = IntPtr.Zero
        smallIcon = IntPtr.Zero
        windowFlags = 0u
        fullScreen = mode = FullScreen
        minimized = false
        maximized = false
        hide = false
        noResizeFrame = false
        borderLessWindow = mode = BorderLess
        hwnd = IntPtr.Zero
        resize = None
    }        
    OS.openWindow name desc
    {
        desc = desc
    }
    
let loop init app =    
    let (load, unload, update, exit), state = init app.desc
    // load
    load state
    // main-loop
    let mutable q = false
    let desc = app.desc
    let time = Stopwatch.StartNew()
    let mutable passed = 0.0
    let mutable inputState = { mouseLeftDown = false; mouseLeftUp = true; mouseX = 0; mouseY = 0; keys = Map.empty}
    
    while not q do
        q <- OS.handleMessages()
        if OS.inputQueue.Count > 0 then
            let q = OS.inputQueue            
            while q.Count > 0 do
                let m = q.Dequeue()                
                match m with
                | OS.MouseLeft w ->
                    inputState <- { inputState with mouseLeftDown = w; mouseLeftUp = not w }
                | OS.MouseMove (x, y)->
                    inputState <- { inputState with mouseX = x; mouseY = y }
                | OS.KeyDown code ->
                    inputState <- { inputState with keys = inputState.keys |> Map.add code true}
                | OS.KeyUp code ->
                    inputState <- { inputState with keys = inputState.keys |> Map.remove code }                    
        if desc.maximized then
            Thread.Sleep 1
        elif desc.resize |> Option.isSome then
            //let (w, h) = desc.resize |> Option.get
            load state 
            desc.resize <- None
        else            
            let t = time.Elapsed.TotalSeconds
            let delta = float32 <| t - passed
            passed <- t
            let delta = if delta >= 0.2f then 0.05f else delta                
            state |> update delta inputState
    state |> unload
    state |> exit