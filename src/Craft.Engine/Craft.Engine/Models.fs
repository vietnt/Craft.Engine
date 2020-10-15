[<AutoOpen>]
module Craft.Engine.Models

open System
open Vortice.DXGI
open Vortice.Direct3D11

type Texture = {
    width            : int
    height           : int 
    srv              : ID3D11ShaderResourceView
}

type RenderTargetView = {
    width            : int
    height           : int
    rtv              : ID3D11RenderTargetView
}

type DepthTarget = {
    texture          : ID3D11Texture2D
    dsv              : ID3D11DepthStencilView
}

type SwapChain = {
    hwnd             : IntPtr
    width            : int
    height           : int
    swapChain        : IDXGISwapChain
    backBuffer       : RenderTargetView
    depth            : DepthTarget
}

type GpuDescription = {
    name                      : string
    memory                    : int
}

type GpuMode =
    | Debug
    | Production

type DepthState =    
    | Less = 0x02
    | Equal = 0x03
    | LessEqual = 0x04
    | Greater = 0x05
    | NotEqual = 0x06
    | GreaterEqual = 0x07 
    | Always = 0x08
    | Write = 0x10
    
type TextureFormat =
    | RGBA16F
    | RGBA16U
    | RGBA8
    | RGB10A2 
    | D24S8

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
    mode                      : GpuMode 
}

[<Struct>]
type MeshData = {        
    count                     : int
    vertexOffset              : int
    indexOffset               : int
    gen                       : int 
}

type MeshPool = {
    gpu                       : GpuContext
    vertexBuffer              : ID3D11Buffer
    indexBuffer               : ID3D11Buffer
    vertexView                : VertexBufferView        
    stride                    : int
    vertexCapacity            : int
    indexCapacity             : int    
    meshes                    : ResizeArray<MeshData>
    mutable vertexPos         : int
    mutable indexPos          : int
    mutable generation        : int
    maxMesh                   : int 
}

type ShaderStage =
    | None = 0
    | Vertex = 1
    | Pixel = 2
    | Compute = 4        

type Shader = {
    vertex                    : ID3D11VertexShader
    pixel                     : ID3D11PixelShader
    compute                   : ID3D11ComputeShader
    inputLayout               : ID3D11InputLayout
}

let trackResource r (gpu: GpuContext) =
    gpu.resources.Add r
    
let trackResources r (gpu: GpuContext) = r |> Seq.iter gpu.resources.Add