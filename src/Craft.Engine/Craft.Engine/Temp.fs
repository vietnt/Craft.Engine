module Craft.Engine.Temp

open System.Threading

#nowarn "9"

open System.Collections.Generic
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System
    
let Size = 256*1024
let BigSize = Size * 4
let MaxRegister = 16 //4MB
let MaxBigRegister = 4 //4MB 

let queue = Queue<_>()
let bigQueue = Queue<_>()

let inline _write i (v: 'a) stride ptr =                    
    let addr = IntPtr.Add(ptr, stride * i) |> NativePtr.ofNativeInt<int> |> NativePtr.toVoidPtr
    let s = Span(addr, 1)
    s.[0] <- v
    
let inline _read i stride ptr =
    let addr = IntPtr.Add(ptr, stride * i) |> NativePtr.ofNativeInt<int> |> NativePtr.toVoidPtr
    let s = Span<'a>(addr, 1)
    s.[0]
    
type NativeArray<'a> (ptr) =
    let stride = sizeof<'a>
    
    [<DefaultValue>]
    val mutable count : int
    
    [<DefaultValue>]
    val mutable isBig : bool
    
    member this.Ptr = ptr
    member this.Stride = stride
    
    member inline this.Item 
        with get i : 'a = this.Ptr |> _read i this.Stride
        and set i (v: 'a) = this.Ptr |> _write i v this.Stride 
            
    interface IDisposable with
        member x.Dispose() =
            if x.isBig then bigQueue.Enqueue ptr else queue.Enqueue ptr    

let buffer = Array.zeroCreate<byte> (MaxRegister * Size + MaxBigRegister * BigSize)
let pinned = GCHandle.Alloc(buffer, GCHandleType.Pinned)
let ptr = pinned.AddrOfPinnedObject()

for i = MaxRegister-1 downto 0 do 
    queue.Enqueue(IntPtr.Add(ptr, i * Size))
    
for i = MaxBigRegister-1 downto 0 do
    bigQueue.Enqueue (IntPtr.Add(ptr, MaxRegister * Size + i * BigSize))

let allocate<'a>() : NativeArray<'a> =
    let ptr = queue.Dequeue()
    new NativeArray<'a> (ptr)
    
let bigAllocate<'a>() : NativeArray<'a> =
    let ptr = bigQueue.Dequeue()
    let n = new NativeArray<'a> (ptr)
    n.isBig <- true
    n 

let inline write i (v: 'a) (a: NativeArray<'a>) = a.[i] <- v
let inline read i (a: NativeArray<'a>) = a.[i]

let appendRange ptr count (a: NativeArray<'a>) =
    let pos = Interlocked.Add(&a.count, count) - count
    let cap = if a.isBig then BigSize/a.Stride else Size / a.Stride
    if a.count >= cap then failwithf "Out of range exception: %A - capacity: %A" a.count cap 
    let str = a.Stride
    for i = 0 to count-1 do
        a.[i + pos] <- ptr |> _read i str    