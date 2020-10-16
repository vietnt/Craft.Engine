module Craft.Engine.MeshPool

open System
open Vortice.DXGI
open Vortice.Direct3D
open Vortice.Direct3D11
open Vortice.Mathematics

    
[<Literal>]
let N = 5
///1-(0.5^(1/N)) = 0.87 = 87% fill rate

[<Literal>]
let MinMeshSize = 64

let create stride capacity (gpu: GpuContext) =
    let isFast = capacity <= 1024*1024 && capacity * stride <= 32*1024*1024
    let mutable vertexDesc = BufferDescription()
    vertexDesc.BindFlags <- BindFlags.VertexBuffer
    vertexDesc.Usage <- Usage.Default // if isFast then Usage.Dynamic else Usage.Default
    vertexDesc.CpuAccessFlags <- CpuAccessFlags.None
    vertexDesc.SizeInBytes <- capacity * stride
    vertexDesc.StructureByteStride <- stride
    let vertex = gpu.device.CreateBuffer vertexDesc
    // index
    let maxIndex = capacity * 4
    let mutable indexDesc = BufferDescription(maxIndex * 4, BindFlags.IndexBuffer ||| BindFlags.UnorderedAccess, Usage.Default, ResourceOptionFlags.None, 4)
    let index = gpu.device.CreateBuffer indexDesc
    //gpu.ctx.CopySubresourceRegion(index, 0, 0, 0, 0, index, 0)
    gpu |> trackResource vertex
    gpu |> trackResource index
    // return 
    {
        gpu = gpu
        vertexView = VertexBufferView(vertex, stride)
        stride = stride
        vertexCapacity = capacity
        indexCapacity = maxIndex
        vertexBuffer = vertex
        indexBuffer = index
        vertexPos = 0
        indexPos = 0
        generation = 0
        meshes = ResizeArray 1024 //nah nah
        maxMesh = capacity*4/MinMeshSize
    }
    
let private checkId id (pool: MeshPool) =
    let i = id%pool.maxMesh
    let g = id/pool.maxMesh        
    if g = pool.generation then
        true
    elif g + 2 <= pool.generation then
        false
    else
        let r = pool.meshes.[i]
        r.gen = g && r.vertexOffset > pool.vertexPos
        
        
let private rand = System.Random()             
        
let private tryFindFreeSlot (pool: MeshPool) =
    let checkSlot i =
        let r = pool.meshes.[i]
        r.gen = pool.generation || (r.gen = pool.generation - 1 && r.vertexOffset > pool.vertexPos)            
    let rec f c =
        let id = rand.Next pool.meshes.Count
        if not <| checkSlot id then ValueSome id
        elif c > 0 then f (c - 1)
        else ValueNone
    if pool.meshes.Count = 0 then ValueNone
    else f N 
    
let add (vertex: _ []) (index: int []) offset vcount icount (pool: MeshPool) =
    let vertexPos =
        let t = pool.vertexPos
        if t + vcount > pool.vertexCapacity then                
            pool.generation <- pool.generation + 1                 
            0
        else t 
    let indexPos =
        let t = pool.indexPos
        if t + icount > pool.indexCapacity then 0
        else t 
    
    let data = {
        vertexOffset = vertexPos
        indexOffset = indexPos
        count = icount
        gen = pool.generation
    }
    
    let ctx = pool.gpu.ctx
    // update vertex buffer
    let box = Box(vertexPos * pool.stride, offset, 0, (vertexPos + vcount)*pool.stride, 1, 1)
    ctx.UpdateSubresource (vertex, pool.vertexBuffer, 0, pool.stride, 0, Nullable(box))
    // update index buffer
    let box = Box(indexPos * 4, offset, 0, (indexPos + icount)*4, 1, 1)
    ctx.UpdateSubresource (index, pool.indexBuffer, 0, 4, 0, Nullable(box))
    
    let count = max MinMeshSize (icount/MinMeshSize*MinMeshSize)
    pool.vertexPos <- vertexPos + count
    pool.indexPos <- indexPos + count
    
    let rec f n data =     
        match pool |> tryFindFreeSlot with
        | ValueSome id ->
            let idx = id % pool.maxMesh
            pool.meshes.[idx] <- data
            idx + pool.generation * pool.maxMesh
        | _ ->
            if pool.meshes.Count >= pool.maxMesh-1 then
                if n > 1 then f (n-1) data
                else failwithf "failed to add mesh, count: %A" pool.meshes.Count
            else
                pool.meshes.Add data
                (pool.meshes.Count - 1) + pool.generation * pool.maxMesh
    f N data
        
let tryGet id (mesh: MeshPool) =
    if mesh |> checkId id then
        let i = id % mesh.maxMesh
        ValueSome mesh.meshes.[i]
    else
        ValueNone
