module Craft.Engine.Game

open System
open System.Diagnostics
open System.Threading
open Craft.Engine

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
    let load' w h load gpu =
        let sw = gpu |> GPU.createSwapChain w h app.desc.hwnd
        load sw 
        sw
    let gpu, (load, unload, update, exit), state = init app.desc
    // load
    let rect = app.desc.windowedRect
    let w = rect.right - rect.left
    let h = rect.bottom - rect.top    
    // main-loop
    let mutable q = false
    let desc = app.desc
    let time = Stopwatch.StartNew()
    let mutable passed = 0.0
    let mutable inputState = { mouseLeftDown = false; mouseLeftUp = true; mouseX = 0; mouseY = 0; keys = Map.empty}
    let mutable swapChain =  gpu |> load' w h (fun sw -> state |> load sw) 
    
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
            let (w, h) = desc.resize |> Option.get
            if swapChain.width <> w || swapChain.height <> h then             
                state |> unload swapChain
                swapChain <- gpu |> load' w h (fun sw -> state |> load sw)              
            desc.resize <- None
        else            
            let t = time.Elapsed.TotalSeconds
            let delta = float32 <| t - passed
            passed <- t
            let delta = if delta >= 0.2f then 0.05f else delta                
            state |> update delta inputState
    state |> unload swapChain
    state |> exit