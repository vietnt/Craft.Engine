namespace Craft.Engine.Compression

open System 
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"

module Fse =
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern uint32 FSE_versionNumber()
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 HUF_compress(IntPtr dst, int64 cap, IntPtr src, int64 size)
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 FSE_compress(IntPtr dst, int64 cap, IntPtr src, int64 size)
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 FSE_compress2(IntPtr dst, int64 cap, IntPtr src, int64 size, uint32 maxSymbol, uint32 tableLog)    
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 HUF_decompress(IntPtr dst, int64 orgSize, IntPtr src, int64 size)
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 FSE_decompress(IntPtr dst, int64 orgSize, IntPtr src, int64 size)
    
    [<DllImport("fse.dll", SetLastError=true)>]
    extern int64 HUF_compress2(IntPtr dst, int64 cap, IntPtr src, int64 size, uint32 maxSymbol, uint32 tableLog)
    
    let HufBlock = 128*1024
    
    let huf_compress (source: byte[]) sourceOffset (dest: byte[]) dstOffset count =
        let mutable size = count
        use src = fixed (&source.[sourceOffset])
        use dst = fixed (&dest.[dstOffset])        
        let mutable srcP = src
        let mutable dstP = dst  
        let mutable cap = dest.Length - dstOffset
        let mutable fs = 7
        let pc = max 0 ((count + HufBlock-1)/HufBlock-1)
        NativePtr.write dstP (byte pc)
        let lastS = count &&& (HufBlock-1)
        NativePtr.write (NativePtr.add dstP 1) (byte (lastS))
        NativePtr.write (NativePtr.add dstP 2) (byte (lastS >>> 8))
        NativePtr.write (NativePtr.add dstP 3) (byte (lastS >>> 16))
        dstP <- NativePtr.add dstP 7 
        for i = 0 to pc do
            let partSize = min (HufBlock) size
            size <- size - partSize
            let compressedSize = int <| HUF_compress (NativePtr.toNativeInt dstP, int64 cap, NativePtr.toNativeInt srcP, int64 partSize)
            //printfn "p%i = %i" i compressedSize
            //if compressedSize = 0 then compressedSize <- partSize 
            NativePtr.write (NativePtr.add dstP -3) (byte(compressedSize&&&255))
            NativePtr.write (NativePtr.add dstP -2) (byte((compressedSize >>> 8)&&&255))
            NativePtr.write (NativePtr.add dstP -1) (byte((compressedSize >>> 16)&&&255))
            if compressedSize = 0 then
                Marshal.Copy(source, sourceOffset + i * HufBlock, NativePtr.toNativeInt dstP, partSize)
            srcP <- NativePtr.add srcP partSize
            dstP <- NativePtr.add dstP (if compressedSize = 0 then partSize else compressedSize + 3)
            fs <- fs + (compressedSize + 3)            
            cap <- cap - compressedSize - 3        
        fs
        
    let huf_decompress (source: byte[]) srcOffset (dest: byte[]) dstOffset count =
        use src = fixed (&source.[srcOffset])
        use dst = fixed (&dest.[dstOffset])
        let pc = int <| NativePtr.read src
        let lastS =
            let a = int (NativePtr.read (NativePtr.add src 1))
            let b = int (NativePtr.read (NativePtr.add src 2))
            let c = int (NativePtr.read (NativePtr.add src 3))
            a + b * 256 + c * 65536
        //printfn "pc         : %i" pc 
        //printfn "last-size  : %i" lastS 
        let mutable srcP = NativePtr.add src 4
        let mutable dstP = dst        
        for i = 0 to pc do
            let orgSize = if i = pc then lastS else HufBlock
            let ps =
                let a = int (NativePtr.read (NativePtr.add srcP 0))
                let b = int (NativePtr.read (NativePtr.add srcP 1))
                let c = int (NativePtr.read (NativePtr.add srcP 2))
                a + b * 256 + c * 65536                 
            //printfn "ps         : %i" ps
            srcP <- NativePtr.add srcP 3
            if ps = 0 then
                Marshal.Copy(NativePtr.toNativeInt srcP, dest, i * HufBlock, orgSize)
            else
                let d = HUF_decompress (NativePtr.toNativeInt dstP, int64 orgSize, NativePtr.toNativeInt srcP, int64 ps)
                ()
                //printfn "p%i        : %i" i d 
            dstP <- NativePtr.add dstP orgSize
            srcP <- NativePtr.add srcP (if ps = 0 then orgSize else ps)  
        pc * HufBlock + lastS        


module Compression =
    ()    