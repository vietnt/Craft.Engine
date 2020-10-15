module Craft.Engine.ConstantBuffer

#nowarn "9"

open System
open Microsoft.FSharp.NativeInterop
open Vortice.Direct3D11
        
[<Literal>]        
let Align = 256

type T = {
    mutable ptr            : nativeptr<float32>
    segments               : int []
    mutable segmentCount   : int
    mutable used           : int
    gpu                    : GpuContext
    resource               : ID3D11Buffer
}

let createPool size (gpu: GpuContext) =
    //let buf = Array.zeroCreate (size + Align)
    //let handle = GCHandle.Alloc(buf, GCHandleType.Pinned)
    //let t = handle.AddrOfPinnedObject() |> int64
    //let ptr = (t + 255L)/256L*256L |> nativeint |> NativePtr.ofNativeInt<float32>
    
    let mutable desc = BufferDescription(size, BindFlags.ConstantBuffer, Usage.Dynamic)
    desc.CpuAccessFlags <- CpuAccessFlags.Write            
    let buffer = gpu.device.CreateBuffer(desc)
    
    {
        //handle = handle
        //raw = buf
        ptr = Unchecked.defaultof<_>
        segments = Array.zeroCreate (size/256)                
        segmentCount = 0
        used = 0
        gpu = gpu
        resource = buffer
    }
    
let reset (buf: T) =
    let mm = buf.gpu.ctx.Map (buf.resource, MapMode.WriteDiscard)
    buf.ptr <- mm.DataPointer |> NativePtr.ofNativeInt<_>
    buf.used <- 0
    buf.segmentCount <- 0

let flush (buf: T) =
    buf.gpu.ctx.Unmap buf.resource
    
let writeF v (buf: T) =
    NativePtr.write (NativePtr.add buf.ptr buf.used) v
    buf.used <- buf.used + 1
                
let write (data: 't) (buf: T) =            
    //let ptr = (NativePtr.add buf.ptr buf.used) |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<'t>
    //NativePtr.write ptr data
    
    let ptr = (NativePtr.add buf.ptr buf.used) |> NativePtr.toVoidPtr
    let size = sizeof<'t>            
    let sp = Span(ptr, size)
    sp.[0] <- data
    let s = (size + 3)/4            
    buf.used <- buf.used + s
    
let newSub (buf: T) =
    buf.used <- (buf.used + 63)/64*64
    buf.segments.[buf.segmentCount] <- buf.used
    buf.segmentCount <- buf.segmentCount + 1
    
let bind slot i (buf: T) =
    let start = buf.segments.[i]
    let next = if i + 1 < buf.segmentCount then buf.segments.[i+1] else buf.used
    let size = ((next - start)*4+63)/64*64
    buf.gpu.ctx.VSSetConstantBuffer1(slot, buf.resource, [| start/4 |], [| size |])
    buf.gpu.ctx.PSSetConstantBuffer1(slot, buf.resource, [| start/4 |], [| size |])