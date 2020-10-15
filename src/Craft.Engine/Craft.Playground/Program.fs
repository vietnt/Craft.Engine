// Learn more about F# at http://fsharp.org

open System.IO
open Craft.Engine
open Craft.Engine.Game
open Craft.Engine.Math
open Vortice.DXGI
open Vortice.Mathematics

let mode =
#if DEBUG
    true
#else
    false
#endif

type State = {
    mutable swapChain    : SwapChain
}

[<Struct>]
type VertexUI = {
    pos        : Vector2
    uv         : Vector2
    color      : Vector4
}

let inline createVertexUI x y u v r g b a = { pos = vec2 x y; uv = vec2 u v; color = vec4 r g b a } 

let init mode windowDesc =
    let gpu = GPU.initGpu mode |> Result.get
    let ctx = gpu.ctx
    
    // allocate persistent resources
    let cbPool = gpu |> ConstantBuffer.createPool (4*1024*1024) //4 MB buffer    
    
    use stream = new FileStream("C:/@temp/ui.shader", FileMode.Open)
    let uiShader = gpu |> GPU.loadShader stream
    
    let uiMesh = gpu |> MeshPool.create 32 1024
    let vts = [|
        createVertexUI 100.f -100.f 0.f 0.f 0.f 0.f 1.0f 1.0f
        createVertexUI 500.f -100.f 1.0f 0.f 0.f 1.0f 0.f 1.0f 
        createVertexUI 100.f -500.f 0.f 1.0f 1.0f 0.f 0.f 1.0f 
        createVertexUI 500.f -500.f 1.0f 1.0f 1.0f 1.0f 1.0f 1.0f 
    |]
    let ids = [| 0; 1; 2; 1; 3; 2 |]
    let id = uiMesh |> MeshPool.add vts ids 0 4 6
    let mutable uiMatrix = Matrix4x4.identity
    
    let state = {
        swapChain = Unchecked.defaultof<_>
    } 
    
    let load (sw: SwapChain) state =
        uiMatrix <- Matrix4x4.createOrtho 0.f (float32 sw.width) (-float32 sw.height) 0.f -100.0f 100.0f
        state.swapChain <- sw 
    
    let unload (sw: SwapChain) state  = ()
    
    let update time (input: InputState) state =                        
        cbPool |> ConstantBuffer.reset
        // setup
        cbPool |> ConstantBuffer.newSub
        cbPool |> ConstantBuffer.write uiMatrix        
        cbPool |> ConstantBuffer.flush
        
        gpu |> GPU.bindShader uiShader
        
        // actual draw
        let swapChain = state.swapChain
        gpu |> GPU.clear swapChain (0.f, 0.f, 0.0f, 1.0f) (0.f, 1uy)
        
        gpu |> GPU.useDepthState (DepthState.Always)
                
        gpu |> GPU.bindMeshPool uiMesh
        cbPool |> ConstantBuffer.bind 0 0 
        gpu |> GPU.draw id        
        
        swapChain |> GPU.present 1 
        
    
    let bye state = ()
    
    gpu, (load, unload, update, bye), state
    
    
[<EntryPoint>]
let main argv = 
    
    Game.create "Hello world" 1920 1080 Game.Windows |> Game.loop (init mode)
    
    0 // return an integer exit code
