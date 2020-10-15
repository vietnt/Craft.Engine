[<RequireQualifiedAccess>]
module Craft.Engine.GPU

open Vortice.Direct3D11

#nowarn "9"

open System
open System.Collections.Generic
open System.IO
open SharpGen.Runtime.Win32
open Vortice.D3DCompiler
open Vortice.DXGI
open Vortice.Direct3D
open Vortice.Direct3D11
open Vortice.Direct3D11.Debug
open Vortice.Direct3D11.Shader
open Vortice.Mathematics
open FSharp.NativeInterop

let features = [|
    FeatureLevel.Level_11_1
    FeatureLevel.Level_11_0
|]

        
let private createDevice debug =
    let rec scanAdapter (factory: IDXGIFactory1) i lst =
        let r, a = factory.EnumAdapters1(i)
        if r.Success then
            (a :: lst) |> scanAdapter factory (i+1)
        else lst 
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
        [] |> scanAdapter factory 0
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
            mode = if debug then Debug else Production
        } |> Success
        
let free (gpu: GpuContext) =        
    for r in gpu.resources do
        r.Dispose()
    gpu.resources.Clear()
    
let private mapTextureFormat (fmt: TextureFormat) =
    match fmt with
    | TextureFormat.RGBA16F -> Format.R16G16B16A16_Float
    | TextureFormat.RGBA16U -> Format.R16G16B16A16_UNorm
    | TextureFormat.RGBA8 -> Format.R8G8B8A8_UNorm
    | TextureFormat.D24S8 -> Format.D24_UNorm_S8_UInt
    | TextureFormat.RGB10A2 -> Format.R10G10B10A2_UNorm
    
    
let createTexture w h fmt (gpu: GpuContext) =
    ()

let loadTexture path (gpu: GpuContext) =
    let tmp = Array.zeroCreate (2048*2048*7/5)
    use st = new FileStream(path, FileMode.Open)
    st.Read(tmp, 0, 16) |> ignore
    let w = BitConverter.ToInt32(tmp, 0)
    let h = BitConverter.ToInt32(tmp, 4)
    let f = BitConverter.ToInt32(tmp, 8)
    let m = BitConverter.ToInt32(tmp, 12)
    
    let m = 1
    let w = w
    let h = h
    
    let dataBoxes = Array.zeroCreate m 
    use pt = fixed(&tmp.[0])
    //let ptr = NativeInterop.NativePtr.toNativeInt pt
    let mutable total = 0
    let mutable currentH = h
    let temp = Array.zeroCreate 4 
    for i = 0 to m-1 do
        //let lastPtr = if i > 0 then sizes.[i-1] else 0
        st.Read(temp, 0, 4) |> ignore
        let size = BitConverter.ToInt32(temp, 0)
        st.Read(tmp, total, size) |> ignore        
        dataBoxes.[i] <- SubresourceData(NativePtr.add pt total |> NativePtr.toNativeInt, size/currentH*4)
        printfn "size: %A, currentH: %A" size (size/currentH)
        currentH <- currentH/2
        total <- total + size
        //sizes.[i] <- size + lastPtr
            
    let fmt = Format.BC7_UNorm_SRgb        
    let desc = Texture2DDescription(fmt, w, h, 1, m, BindFlags.ShaderResource, Usage.Default, CpuAccessFlags.None)
    let texture = gpu.device.CreateTexture2D(desc, dataBoxes)
    
    let viewDesc = ShaderResourceViewDescription(texture, ShaderResourceViewDimension.Texture2D, fmt, 0, m)
    let view = gpu.device.CreateShaderResourceView(texture, Nullable(viewDesc))
    gpu |> trackResource texture
    gpu |> trackResource view
    { srv = view; width = w; height = h }
    
let createDepthView width height (gpu: GpuContext) =
    let device = gpu.device
    let dt =
        let mutable dd = Texture2DDescription(Width = width, Height = height, Format = Format.R24G8_Typeless, ArraySize = 1)
        dd.MipLevels <- 1
        dd.SampleDescription <- SampleDescription(1, 0)
        dd.Usage <- Usage.Default
        dd.BindFlags <- BindFlags.DepthStencil
        dd.CpuAccessFlags <- CpuAccessFlags.None
        dd.OptionFlags <- ResourceOptionFlags.None
        device.CreateTexture2D(dd)    
    let dv =
        let mutable dd = DepthStencilViewDescription()
        dd.Format <- Format.D24_UNorm_S8_UInt
        dd.ViewDimension <- DepthStencilViewDimension.Texture2D
        device.CreateDepthStencilView(dt, Nullable(dd))
    // track
    //gpu |> trackResource dt
    //gpu |> trackResource dv 
    { texture = dt; dsv = dv }    
    
let createRenderTarget buffer w h (gpu: GpuContext) =
    let mutable d = RenderTargetViewDescription()
    d.ViewDimension <- RenderTargetViewDimension.Texture2D
    let rtv = gpu.device.CreateRenderTargetView(buffer, Nullable(d))                
    { rtv = rtv; width = w; height = h }
        
let createSwapChain width height handle (gpu: GpuContext) =         
    let mutable swd = SwapChainDescription()
    swd.BufferDescription.Width <- width
    swd.BufferDescription.Height <- height
    swd.BufferDescription.Format <- Format.B8G8R8A8_UNorm
    swd.SampleDescription.Count <- 1
    swd.SampleDescription.Quality <- 0
    swd.BufferCount <- 1
    swd.Usage <- Usage.RenderTargetOutput
    swd.OutputWindow <- handle
    swd.IsWindowed <- RawBool true
    swd.SwapEffect <- SwapEffect.Discard
    let _,factory = DXGI.CreateDXGIFactory1() 
    let swapChain = factory.CreateSwapChain (gpu.device, swd)
    factory.Dispose()
    let factory = swapChain.GetParent<IDXGIFactory1>()
    factory.MakeWindowAssociation(handle, WindowAssociationFlags.IgnoreAltEnter)
    factory.Dispose()
    //gpu.resources.Add swapChain
    let rtv = gpu |> createRenderTarget (swapChain.GetBuffer(0)) width height
    let dv = gpu |> createDepthView width height
    let ctx = gpu.ctx
    ctx.OMSetRenderTargets(rtv.rtv, dv.dsv)                   
    ctx.RSSetViewport(0.f, 0.f, float32 width, float32 height, 0.0f, 1.f)
    { swapChain = swapChain; width = width; height = height; hwnd = handle; backBuffer = rtv; depth = dv}
    
let destroySwapChain (sw: SwapChain) =
    let raw = sw.swapChain
    sw.depth.texture.Dispose()
    sw.depth.dsv.Dispose()
    raw.Dispose()
    
let present interval (sw: SwapChain) =
    sw.swapChain.Present(interval, PresentFlags.None) |> ignore
    
let clear (sw: SwapChain) (r, g, b, a) (d, s) (gpu: GpuContext) =
    gpu.ctx.ClearRenderTargetView (sw.backBuffer.rtv, Color4(r, g, b, a))
    gpu.ctx.ClearDepthStencilView(sw.depth.dsv, DepthStencilClearFlags.Stencil ||| DepthStencilClearFlags.Depth, d, s)
    
let mutable private currentMeshPool = Unchecked.defaultof<_>
    
let bindMeshPool (pool: MeshPool) (gpu: GpuContext) =
    let ctx = gpu.ctx
    ctx.IASetVertexBuffers(0, pool.vertexView)
    ctx.IASetIndexBuffer(pool.indexBuffer, Format.R32_UInt, 0)
    currentMeshPool <- pool
    
let draw id (gpu: GpuContext) =
    match currentMeshPool |> MeshPool.tryGet id with
    | ValueNone -> ()
    | ValueSome m ->
        gpu.ctx.DrawIndexed (m.count, m.indexOffset, m.vertexOffset)
        
let private createInputLayout (code: byte[]) (device: ID3D11Device) =
    let r,ref = Compiler.Reflect<ID3D11ShaderReflection>(code)
    r.CheckError()
    try
        let desc = ref.Description
        let inputs = [|
            for i = 0 to desc.InputParameters-1 do
                let ps = ref.GetInputParameterDescription i
                let mutable input = InputElementDescription()
                input.SemanticName <- ps.SemanticName
                input.SemanticIndex <- ps.SemanticIndex
                input.Slot <- 0
                input.AlignedByteOffset <- 0xffffffff
                input.Classification <- InputClassification.PerVertexData
                if ps.ComponentType = RegisterComponentType.Float32 then
                    input.Format <-
                        if int ps.UsageMask = 1 then Format.R32_Float
                        elif int ps.UsageMask <= 3 then Format.R32G32_Float
                        elif int ps.UsageMask <= 7 then Format.R32G32B32_Float
                        else Format.R32G32B32A32_Float
                input
        |]
        device.CreateInputLayout (inputs, code)
    finally
        ref.Dispose()
        

let loadShader (stream: Stream) (gpu: GpuContext) =
    use br = new BinaryReader(stream)
    let n = br.ReadInt32()
    let stages = [
        for _ = 0 to n-1 do
            let stage =  br.ReadInt32() |> LanguagePrimitives.EnumOfValue
            let rawSize = br.ReadInt32()        
            let buf = br.ReadBytes rawSize
            yield stage,  buf
    ]
    
    let mutable v = Unchecked.defaultof<_>
    let mutable p = Unchecked.defaultof<_>
    let mutable il = Unchecked.defaultof<_>
    
    for stage, buf in stages do
        match stage with
        | ShaderStage.Pixel ->
            p <- gpu.device.CreatePixelShader(buf)                
            gpu.resources.Add p                
        | ShaderStage.Vertex ->
            v <- gpu.device.CreateVertexShader(buf)
            try                
                il <- gpu.device |> createInputLayout buf
                gpu.resources.Add il
            with
            | e -> ()
            gpu.resources.Add v            
    {
        vertex = v
        pixel = p
        compute = Unchecked.defaultof<_>
        inputLayout = il
    }
    
let bindShader (shader: Shader) (gpu: GpuContext) =
    let ctx = gpu.ctx
    ctx.IASetInputLayout shader.inputLayout
    ctx.IASetPrimitiveTopology PrimitiveTopology.TriangleList
    ctx.VSSetShader(shader.vertex)
    ctx.PSSetShader(shader.pixel)
    
let bindShaderResource (stage: ShaderStage) slot srv (gpu: GpuContext) =
    match stage with
    | ShaderStage.Pixel -> gpu.ctx.PSSetShaderResource (slot, srv)
    | ShaderStage.Vertex -> gpu.ctx.VSSetShaderResource (slot, srv)
    | _ -> ()
        
let useDepthState (state: DepthState) (gpu: GpuContext) =
    if state <> gpu.activeDepthState then 
        let comp = int state &&& 0xF
        let write = if state &&& DepthState.Write = DepthState.Write then 1 else 0
        let id = (comp - 2)*2 + write
        let ds = gpu.depthStates.[id]
        gpu.ctx.OMSetDepthStencilState(ds)
        gpu.activeDepthState <- state    