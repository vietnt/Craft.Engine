module Craft.Engine.GPU

open System
open System.IO
open SharpGen.Runtime.Win32
open Vortice.D3DCompiler
open Vortice.DXGI
open Vortice.Direct3D
open Vortice.Direct3D11
open Vortice.Direct3D11.Debug
open Vortice.Direct3D11.Shader
open Vortice.Mathematics

let features = [|
    FeatureLevel.Level_11_1
    FeatureLevel.Level_11_0
|]

type GpuDescription = {
    name                      : string
    memory                    : int
}

type DepthState =    
    | Less = 0x02
    | Equal = 0x03
    | LessEqual = 0x04
    | Greater = 0x05
    | NotEqual = 0x06
    | GreaterEqual = 0x07 
    | Always = 0x08
    | Write = 0x10

type GpuContext = {
    device                    : ID3D11Device
    ctx                       : ID3D11DeviceContext1
    adapter                   : IDXGIAdapter1
    factory                   : IDXGIFactory1
    desc                      : GpuDescription
    resources                 : ResizeArray<IDisposable>
    screenResources           : ResizeArray<IDisposable>
    depthStates               : ID3D11DepthStencilState []
    mutable activeDepthState  : DepthState 
}

let private createDevice debug =
    let _,factory = DXGI.CreateDXGIFactory1()
    let flags =
        if debug then DeviceCreationFlags.Debug
        else DeviceCreationFlags.None            
    let tryGetGpuSpec (adapter: IDXGIAdapter1) =
        let desc = adapter.Description1
        let mem = int64 desc.DedicatedVideoMemory / 1024L / 1024L
        if desc.Flags &&& AdapterFlags.Software = AdapterFlags.None && mem >= 1536L then                
            printfn "-- adapter: %A - %A MB" desc.Description mem
            let r, device, fsOut, deviceContext = D3D11.D3D11CreateDevice (adapter, DriverType.Unknown, flags, features)            
            if r.Success then                
                device.Dispose()
                deviceContext.Dispose()
                Some (adapter, fsOut, int32 (int64 desc.DedicatedVideoMemory/1024L/1024L))
            else
                None
        else
            adapter.Dispose()
            None                
    let best = //sort by feature level then memory_size
        factory.Adapters1
        |> Seq.choose tryGetGpuSpec
        |> Seq.sortWith (fun (_,x0,y0) (_,x1,y1) -> if x0 = x1 then y0 - y1 else int x1 - int x0)
        |> Seq.tryHead        
    //let best = Some (factory.EnumAdapters1().[0], 1, 1)        
    match best with
    | None -> Failure "Can't find DirectX11 Device"
    | Some (adapter, _, _) ->            
        let r, device, fsOut, deviceContext = D3D11.D3D11CreateDevice (adapter, DriverType.Unknown, flags, features)
        if r.Success then                
            if debug then        
                use queue = device.QueryInterface<ID3D11InfoQueue>()
                queue.PushEmptyStorageFilter()                                    
            let f = device.CheckFeatureSupport<FeatureDataD3D11Options>(Feature.D3D11Options)                        
            printfn "Constant Buffer offset    : %A" f.ConstantBufferOffsetting
            printfn "Constant Buffer Partial   : %A" f.ConstantBufferPartialUpdate
            printfn "CB MAP_NO_OVERWRITE       : %A" f.MapNoOverwriteOnDynamicConstantBuffer
            printfn "Resource Sharing          : %A" f.ExtendedResourceSharing
            printfn "flags                     : %A" adapter.Description1.Flags
            
            let ctx1 = deviceContext.QueryInterface<ID3D11DeviceContext1>()                                
            factory.Dispose()
            
            Success (factory, adapter, device, ctx1)
        else                
            Failure "Can't create DX11 device"                
            
let private getDeviceDesc (adapter: IDXGIAdapter1) =
    let d = adapter.Description1        
    let s = (d.DedicatedVideoMemory |> int64)/1024L/1024L |> int32        
    {
        memory = (s + min 255 (s/16))/256*256
        name = d.Description
    }            

let initGpu debug =
    match createDevice debug with
    | Failure s -> Failure s
    | Success (factory, adapter, device, deviceContext) ->
        let res = ResizeArray [
            device :> IDisposable
            deviceContext :> IDisposable
            adapter :> IDisposable
            //factory :> IDisposable
        ]
        res.Capacity <- 1024
        //let blendState, depthState, rasterState = createDefaultStates device
        
        let depths = [|
            for i = 2 to 8 do
                let enable = i <> int DepthState.Always
                //let write = i &&& int DepthState.Write = int DepthState.Write
                for write in [false; true] do 
                    let mutable desc = DepthStencilDescription(enable, write)
                    desc.DepthFunc <- LanguagePrimitives.EnumOfValue (i &&& 0xF)
                    desc.DepthWriteMask <- DepthWriteMask.All
                    device.CreateDepthStencilState desc
        |]
        deviceContext.OMSetDepthStencilState depths.[1] //LESS/WRITE
        {            
            device = device
            ctx = deviceContext
            desc = getDeviceDesc adapter
            adapter = adapter
            factory = Unchecked.defaultof<_>
            resources = res
            screenResources = ResizeArray(8)
            depthStates = depths
            activeDepthState = DepthState.Less ||| DepthState.Write //
        } |> Success
        
let free (gpu: GpuContext) =        
    for r in gpu.resources do r.Dispose()
    gpu.resources.Clear()
