// Learn more about F# at http://fsharp.org

open System.Runtime.InteropServices
#nowarn "9"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Runtime.Intrinsics.X86
open Microsoft.FSharp.NativeInterop


module BitStream =
    
    type Stream = {
        pos            : int 
    }
        
//let SlotCount = 32
let Depth = 4096
let ShortDistance = 128*1024
let nextSize = 128*1024
let mask = 256*256*256-1
let ht =
    let t = Array.zeroCreate (mask+1)
    //for i = 0 to t.Length-1 do t.[i] <- -1
    Array.Fill(t, -1)
    t
    
let ht5 =
    let t = Array.zeroCreate (mask+1)
    //for i = 0 to t.Length-1 do t.[i] <- -1
    Array.Fill(t, -1)
    t     
    //Array.init ((mask+1)*SlotCount) (fun _ -> -1)
let next = Array.init (nextSize) (fun _ -> -1)
let next5 = Array.init (nextSize) (fun _ -> -1)
let nextMask = next.Length-1
//let hc = Array.zeroCreate (mask+1)

let compressLZ (input: byte []) =
    
    use inputP = fixed(&input.[0])

    let inline readB x =
        NativePtr.read (NativePtr.add inputP x)
        
    let inline read32 x =
        NativePtr.read (NativePtr.add inputP x |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<int>)        
        

    //static const U32 prime3bytes = 506832829U;
    //static U32    ZSTD_hash3(U32 u, U32 h) { return ((u << (32-24)) * prime3bytes)  >> (32-h) ; }
        
    let inline hash off =
        //let x = int (readB off) + int (readB (off+1)) * 256 + int (readB (off+2)) * 65536 
        //let x = int input.[off+2] * 65536 + int input.[off+1] * 256 + int input.[off+0]
        //let x = (read32 off) >>> 8
        //(x * 506832829 ) &&& mask  
        //(read32 off) &&& mask
        //((((read32 off)&&&0xFFFFFF) * 506832829 ) >>> 16) &&& mask
        (((read32 off) * 506832829 ) >>> 8) &&& mask
//        let x = int64 <| read32 off
//        let y = int64 <| read32 (off + 1)
//        let mutable t = x ^^^ (y >>> 33)
//        t <- t * 0xff51afd7ed558ccdL
//        t <- t ^^^ (t >>> 33)
//        t <- t * 0xc4ceb9fe1a85ec53L
//        t <- t ^^^ (t >>> 33)
//        (int t >>> 2) &&& mask

    let inline hash5 off =
        let x = int64 <| read32 off
        let y = int64 <| read32 (off + 1)
        let mutable t = x ^^^ (y >>> 33)
        t <- t * 0xff51afd7ed558ccdL
        t <- t ^^^ (t >>> 33)
        t <- t * 0xc4ceb9fe1a85ec53L
        t <- t ^^^ (t >>> 33)
        (int t >>> 8) &&& mask    
        
        //((x * 506832829) >>> 2) &&& mask 
        //((x * 506832829 ) >>> shift) &&& mask                 
        
    let inline eq4 x y =  read32 x = read32 y
    let inline eq5 x y =
        (read32 x - read32 y) + (read32 (x+1) - read32 (y+1))*253 = 0 
        //eq4 x y && read32 (x+1) = read32 (y+1)
    
    let inline count x y =
        let l = min (input.Length - x) (input.Length - y)
        let mutable i = 4
        while i + 4 < l && eq4 (x + i) (y + i) do
            i <- i + 4
        while i < l && readB (x + i) = readB (y + i) do
            i <- i + 1 
        i
                
    let inline eval x off = count x off
    
    use nextP = fixed &next.[0]
    use next5P = fixed &next5.[0]
    
    let inline readNext w = NativePtr.read (NativePtr.add nextP w)
    let inline readNext5 w = NativePtr.read (NativePtr.add next5P w)
    
    let push off =
        let h = hash off 
        let w = ht.[h]
        ht.[h] <- off                
        NativePtr.write (NativePtr.add nextP (off &&& nextMask)) w
        let h5 = hash5 off
        let w2 = ht5.[h5]
        ht5.[h5] <- off 
        NativePtr.write (NativePtr.add next5P (off &&& nextMask)) w2            
        
    let inline findBest off =
        //let mutable ptr = ht.[h]        
        let mutable best = 0
        let mutable bestOff = -1
        
//        while ptr >= 0 && ptr + nextMask > off && best = 0 do             
//            if eq4 off ptr then 
//                let c = eval off ptr
//                if c > best then
//                    let d = off - ptr
//                    if best > 4 || d < ShortDistance then 
//                        best <- c
//                        bestOff <- ptr                
//            ptr <- readNext (ptr &&& nextMask)
            
        let mutable ptr = ht5.[hash5 off] 
        while ptr >= 0 && ptr + nextMask > off do             
            if eq5 off ptr then 
                let c = eval off ptr
                if c > best then
                    best <- c
                    bestOff <- ptr                
            ptr <- readNext5 (ptr &&& nextMask)
            
//        if best = 0 then
//            ptr <- ht.[hash off]
//            while ptr >= 0 && ptr + nextMask > off && best < 4 do //&& best = 0 do             
//                if eq4 off ptr then 
//                    let c = eval off ptr
//                    if c > best then
//                        best <- c
//                        bestOff <- ptr                
//                ptr <- readNext (ptr &&& nextMask)            
        
        best, bestOff
        
    let seqs = ResizeArray(input.Length/8)        
    
    let mutable lastL = 0
    
    let mutable i = 0
    let mutable nextPtr = 0
    
    //for i = 0 to input.Length-3 do
    while i < input.Length-6 do
        
        let n0, b0 = findBest i
        if n0 >= 4 then         
//            let n0, b0 =
//                let mutable bn = n0
//                let mutable bb = b0
//                let mutable bi = 0
//                for j = 1 to 0 do
//                    let n, b = findBest (hash (i+j)) (i+j)
//                    if n > bn then
//                        bn <- n
//                        bb <- b
//                        bi <- j
//                i <- bi + i 
//                bn, bb
                    
            //seqs.Add (i - lastL, i + 1 - b0, n0)
            seqs.Add(i - lastL)
            seqs.Add(i - b0)
            seqs.Add(n0)
            let m = min (n0-1) (input.Length-4-i)
            for j = 0 to m do
                let addr = j + i 
                push addr
            i <- i + n0 
            lastL <- i                
        else
            push i
            i <- i + 1
        
    if input.Length > lastL then
        seqs.Add(input.Length - lastL)
        seqs.Add 0
        seqs.Add 0 
                
    seqs
    
let histogram (input: byte[]) count (freqs: int[]) =    
    for i = 0 to count-1 do
        let t = int input.[i]
        freqs.[t] <- freqs.[t] + 1
        
[<Literal>]        
let MaxSymbolValue = 255

[<Literal>]        
let DefaultMemoryUsage = 13

[<Literal>]        
let MaxMemoryUsage = 14

[<Literal>]        
let MinTableLog = 5

let MaxTableLog = MaxMemoryUsage - 2 


let countBit n =    
    32 - int (Lzcnt.LeadingZeroCount (uint32 n ))
//    let t = [| 0;  9;  1; 10; 13; 21;  2; 29; 11; 14; 16; 18; 22; 25;  3; 30; 8; 12; 20; 28; 15; 17; 24;  7; 19; 27; 23;  6; 26;  5;  4; 31 |]
//    let mutable v = uint32 n
//    v <- v ||| (v >>> 1)
//    v <- v ||| (v >>> 2)
//    v <- v ||| (v >>> 4)
//    v <- v ||| (v >>> 8)
//    v <- v ||| (v >>> 16)
//    let i = (v*0x07C4ACDDu) >>> 27
//    printfn "%i - %A - %i" n t.[int i] (31 - int (Lzcnt.LeadingZeroCount (uint32 n))) 
//    t.[int i ]
//    if n <= 1 then 1
//    else 1 + countBit (n/2)

let optimalTableLog maxTableLog n maxSymbolValue =
    let maxBitsSrc = countBit (n-1) - 2
    let minBits = min (countBit maxSymbolValue + 2) (countBit n + 1)
    let mutable r = maxTableLog
    if r = 0 then r <- DefaultMemoryUsage - 2
    r <- min maxBitsSrc r
    r <- max minBits r
    r <- max MinTableLog r
    r <- min MaxTableLog r
    r

let normalizeCount tableLog (norm: int []) (count: int []) total maxSymbolValue =    
    let rtbTable = [| 0; 473195; 504333; 520860; 550000; 700000; 750000; 830000  |] 
    let scale = 62 - tableLog
    let step = (1UL <<< 62) / uint64 total
    let vStep = 1UL <<< (scale - 20)
    let mutable remains = 1 <<< tableLog
    let mutable largest = 0
    let mutable largestP = 0s
    let low = total >>> tableLog
    for s = 0 to maxSymbolValue do 
        if count.[s] = 0 then
            norm.[s] <- 0
        elif count.[s] <= low then
            norm.[s] <- -1
            remains <- remains - 1
        else
            let mutable proba = int16 ((uint64 count.[s] * step) >>> scale)
            if proba < 8s then
                let rtb = vStep * uint64 rtbTable.[int proba]
                if uint64 count.[s] * step - (uint64 proba <<< scale) > rtb then 
                    proba <- proba + 1s
            if proba > largestP then
                largestP <- proba
                largest <- s
            norm.[s] <- int32 proba
            remains <- remains - int32 proba
        if -remains >= (norm.[largest] >>> 1) then
            ///corner case
            failwith "not implemented"
        else
            norm.[largest] <- norm.[largest] + remains
    for s = 0 to maxSymbolValue do
        printfn "%3i %4i " s norm.[s]
    
[<Struct>]            
type SymbolRecord = {
    mutable nBits        : int
    mutable findState    : int 
}            
            
let buildTable (norm: int[]) (maxSymbol) tableLog =
    let tableSize = 1 <<< tableLog
    let tableMask = tableSize - 1
    let step = (tableSize >>> 1) + (tableSize >>> 3) + 3
    let cumul = Array.zeroCreate (MaxSymbolValue + 2)
    let tableSymbol = Array.zeroCreate tableSize
    let mutable high = tableSize - 1
    let tableU16 = Array.zeroCreate tableSize
    let symbols = Array.zeroCreate<SymbolRecord> tableSize
    
    cumul.[0] <- 0
    for u = 1 to maxSymbol+1 do
        if norm.[u-1] = -1 then
            cumul.[u] <- cumul.[u-1] + 1
            tableSymbol.[high] <- u - 1
            high <- high - 1
        else
            cumul.[u] <- cumul.[u-1] + norm.[u-1]
    cumul.[maxSymbol+1] <- tableSize + 1
    
    // spread
    let mutable pos = 0 
    for s = 0 to maxSymbol do
        let freq = norm.[s]
        for n = 0 to freq-1 do
            tableSymbol.[pos] <- s
            pos <- (pos + step) &&& tableMask
            while pos > high do
                pos <- (pos + step) &&& tableMask
    if pos <> 0 then failwith "pos should be 0"
    
    // build
    for u = 0 to tableSize-1 do
        let s = tableSymbol.[u]         
        tableU16.[cumul.[s]] <- uint16 (tableSize + u)
        cumul.[s] <- cumul.[s] + 1
    // transform table
    let mutable total = 0
    for s = 0 to maxSymbol do
        match norm.[s] with
        | 0 -> symbols.[s].nBits <- ((tableLog + 1) <<< 16) - (1 <<< tableLog)
        | -1
        | 1 ->
            symbols.[s].nBits <- (tableLog <<< 16) - (1 <<< tableLog)
            symbols.[s].findState <- total - 1
            total <- total + 1
        | x ->
            let maxBO = tableLog - countBit (x - 1)
            let minSP = x <<< maxBO
            symbols.[s].nBits <- (maxBO <<< 16) - minSP
            symbols.[s].findState <- total - x
            total <- total + x
            
//    for s = 0 to maxSymbol do
//        let b = symbols.[s].nBits
//        let w = (b + ((1 <<< 16) - 1)) >>> 16 
//        printfn "%3i:  maxBits = %i, state = %i" s w symbols.[s].findState
    for i = 0 to tableSize-1 do
        let s = tableSymbol.[i]
        let b = symbols.[s].nBits
        let w = (b + ((1 <<< 16) - 1)) >>> 16 
        printfn "%3i %3i: maxBits = %i, state = %i" i s w symbols.[s].findState
        

let compress (input: byte[]) n =    
    let norm = Array.zeroCreate (MaxSymbolValue + 1)
    let count = Array.zeroCreate (MaxSymbolValue + 1)    
    histogram input n count
    let maxCount = count |> Seq.max
    match maxCount with
    | n -> 0
    | 1 -> 1
    | x when x < n/128 -> 0
    | _ ->
        
        0 
        
let histogram4 (input: byte[]) count (freqs: int[]) =
    use ptr = fixed &input.[0]
    let mutable p = NativeInterop.NativePtr.toNativeInt ptr |> NativeInterop.NativePtr.ofNativeInt<int>
    for i = 0 to count/4-1 do
        let c = NativeInterop.NativePtr.read p
        freqs.[c&&&255] <- freqs.[c&&&255] + 1
        freqs.[(c>>>8)&&&255] <- freqs.[(c>>>8)&&&255] + 1
        freqs.[(c>>>16)&&&255] <- freqs.[(c>>>16)&&&255] + 1
        freqs.[(uint32 c)>>>24 |> int] <- freqs.[(uint32 c)>>>24 |> int] + 1
        p <- NativeInterop.NativePtr.add p 1 
        
//    for i = 0 to count-1 do
//        let t = int input.[i]
//        freqs.[t] <- freqs.[t] + 1            


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
    
[<EntryPoint>]
let main argv =
    
//    printfn "%A" <| Fse.FSE_versionNumber()
//    
//    let bytes = File.ReadAllBytes(@"F:\adam_cross.dds")
//    let buf = Array.zeroCreate<byte> 60_000
//    Array.Copy(bytes, 0, buf, 0, buf.Length)
//    
//    let temp = Array.zeroCreate<byte> (buf.Length*2)
//    
//    use bufPtr = fixed(&buf.[0])
//    use tempPtr = fixed(&temp.[0])
//    let size = int32 <| Fse.HUF_compress2 (tempPtr |> NativePtr.toNativeInt, int64 temp.Length, bufPtr |> NativePtr.toNativeInt, int64 buf.Length, 255u, 12u)
//    printfn "huf: %A" size
//    printfn "%A" temp.[size-1]
//    printfn "%A" temp.[size]
//    printfn "%A" temp.[size+1]
//    use output = new MemoryStream(temp.Length)
//    printfn "max: %A" (buf |> Seq.max)
//    use g = new GZipStream(output, CompressionLevel.Optimal, true)
//    g.Write (buf, 0, 60_000)
//    g.Dispose()
//    
//    printfn "gzip: %A" output.Position    
//    exit 0 
    
    
//    let norm = Array.zeroCreate 54
//    
//    let offsetCount = [| 1; 4; 3; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1; 1; 1;
//          1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
//          1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;-1;-1;
//         -1;-1;-1;-1;-1 |]        
//    
//    //optimalTableLog 6 1023456 52 |> printfn "%A"
//    
//    //normalizeCount 8 norm offsetCount 1023456 52
//    
//    buildTable offsetCount 52 6
//    
//    exit 0 
    
    //let bytes = File.ReadAllBytes(@"F:\adam_bc6.dds")
    //let bytes = File.ReadAllBytes(@"F:\@Temp\__testCompress\ooffice")
    //let bytes = File.ReadAllBytes(@"E:\out.dds")
    //let bytes = File.ReadAllBytes(@"F:\adam_cross.dds")
    let bytes = File.ReadAllBytes(@"F:\@Temp\test\temp_dif.bin")
//    let m = new MemoryStream()
//    let bw = new BinaryWriter(m)
//    //let bytes = [| 0; 0; 1; 2; 1; 2 |] |> Array.collect (fun x -> Convert.)
//    for x in [| 0; 1; 2; 1; 2; 1; 2; 1; 2; 3; 4; 3; 4 |] do
//        bw.Write(x)
//    let bytes = m.ToArray()
    //preprocess bytes
    let freqs = Array.zeroCreate 256 
    let w = Stopwatch.StartNew()
    histogram bytes bytes.Length freqs
    w.Stop()
    printfn "%A s" w.Elapsed.TotalSeconds
    let w = Stopwatch.StartNew()
    let ts = compressLZ bytes    
    w.Stop()
    printfn "%A s" w.Elapsed.TotalSeconds
    
    let seqs = ResizeArray(ts.Count/3)
    for i = 0 to ts.Count/3-1 do
        seqs.Add(ts.[i*3+0], ts.[i*3+1], ts.[i*3+2])
    
    printfn "count = %A" seqs.Count
    
    let mutable l = 0
    let mutable s = 0
    let mutable cp = 0
    
    printfn "max length: %A" (seqs |> Seq.map (fun (_,_,z) -> z) |> Seq.max)
    
    let estLength2 n =
        if n <= 7+4 then 4 //6        
        elif n <= 7+4+4 then 6 //16;17
        elif n <= 7+4+4+8 then 7
        elif n <= 7+4+4+8+16 then 8
        elif n <= 7+4+4+8+16+32 then 9
        elif n <= 7+4+4+8+16+32+64 then 10
        elif n <= 7+4+4+8+16+32+128+1024 then 14
        elif n <= 7+4+4+8+16+32+128+1024+8192 then 17
        else 24  
        
//    let estLength n =
//        if n <= 31+4 then 5 //6
//        elif n <= 31+4+8 then 6 //16;17
//        elif n <= 31+4+8+8 then 7
//        elif n <= 31+4+8+8+16 then 8
//        elif n <= 31+4+8+8+16+16 then 9
//        elif n <= 31+4+8+8+16+16+64 then 10
//        elif n <= 31+4+8+8+16+16+64+128 then 11
//        elif n <= 31+4+8+8+16+16+64+128+256 then 12
//        elif n <= 31+4+8+8+16+16+64+128+256+512 then 13
//        else 24         
        
    let rec estOff2 n =
        if n > 1 then 1 + estOff2 (n/2)        
        else 5 //31
        
    let mutable mode = 1 //exepct L,M
    
    let inline normal4 o =
        if o = 0 then 0
        elif o < 16 then 4
        elif o < 256+16 then 8
        elif o < 256+16+4096 then 12
        else 16
        
    let inline bigO o =
        if o < 2048 then 12
        else 20
        
    let estLength3 l n o =                
        match mode with
        | 1 -> //L or M
            mode <-
                if l > 0 && n = 0 then 0 //after literal elif
                else 1 //after match            
//            2 + 2 //length of L, M
//            + normal4 l
//            + normal4 n
//            + (if n = 0 then 0 else bigO o)
            4 //control: 0-3/literal 12,13,14,15
            + (normal4 l)             
            + (if n < 11 then 0 else normal4 (n-11))
            + (if n = 0 then 0 else bigO o)
        | _ -> //M or RM 
            mode <- 1
            2 + 2 //N, O mode
            + (if n < 11 then 0 else normal4 (n-11))
            + (if o < 4 then 0 else bigO o)
            
    let estLengthX l n o =
        let lorM l n o =
            mode <- if l > 0 then 0 else 1
            4
            + (normal4 l)
            + (if n < 11 then 0 else normal4 (n-11))
            + (if n = 0 then 0 else bigO o)
        let mOrR n o =
            mode <- 1
            4
            + (if n < 11 then 0 else normal4 (n-11))
            + (if o < 4 then 0 else bigO o)
        
        match mode with
        | 1 ->
            if l > 0 then
                lorM l 0 0 + mOrR n o
            else
                lorM 0 n o
        | _ ->
            mOrR n o
                        
    let estOff n =
        if n < 2048 then 12
        else 20
        
    let estLength l m o =
        estLength2 l + estLength2 m + estOff o 
    
    //let offH = Queue<int>()
    let offH = LinkedList<int>()
    
    let mutable fs = 0        
    
    for (ls, off, ms) in seqs do
        //printfn "%A" (ls; off; ms)
        l <- l + ls
        //s <- s + 3
        
        fs <- fs + ls + ms                
        
        let off =
            let mutable i = 0
            let mutable r = off + 4
//            for x in offH do
//                if x = off && r = off + 4 then r <- i
//                i <- i + 1
//            if r >= 4 then 
//                if offH.Count >= 4 then  offH.Dequeue() |> ignore
//                offH.Enqueue off
            let mutable node = offH.First
            let mutable i = 0 
            while not (isNull node) && r >= 4 do
                if node.Value = off then
                    r <- i
                    if node <> offH.First then
                        offH.Remove node
                        offH.AddFirst node
                node <- node.Next
                i <- 0
            if r >= 4 then
                if not (isNull offH.Last)  then  offH.RemoveLast()
                offH.AddFirst (LinkedListNode(off))            
            r
        
        cp <- cp + ms        
        s <- s +  estLengthX ls ms off //+ estOff off
        
    let lmem = new MemoryStream(l)
    let gmem = new GZipStream(lmem, CompressionLevel.Optimal, true)        
    let mutable pos = 0
    let mutable total = 0
    for i = 0 to ts.Count/3-1 do
        let l = ts.[i*3+0]
        let m = ts.[i*3+2]
        if l > 0 then
            gmem.Write(bytes, pos, l)
            total <- total + l 
        pos <- pos + l + m        
    gmem.Dispose()
    lmem.Position <- 0L
   
    let literalBuf = Array.zeroCreate l
    let w = Stopwatch.StartNew()
    let d = new GZipStream(lmem, CompressionMode.Decompress)
    let r = d.Read(literalBuf, 0, l)    
    w.Stop()
    printfn "decompression take: %i ms" w.ElapsedMilliseconds    
    printfn "read: %i/%i/%i" r l total 
    if r < l then d.Read(literalBuf, r, int l-r) |> printfn "re-read: %i"
    
    let inline encodeLit s =
        if s < 16 then s, 0
        elif s < 18 then 16, 1
        elif s < 20 then 17, 1
        elif s < 22 then 18, 1
        elif s < 24 then 19, 1
        elif s < 28 then 20, 2
        elif s < 32 then 21, 2
        elif s < 40 then 22, 3
        elif s < 48 then 23, 3
        elif s < 64 then 24, 4
        elif s < 128 then 25, 6
        elif s < 256 then 26, 7
        elif s < 512 then 27, 8
        elif s < 1024 then 28, 9
        elif s < 2048 then 29, 10
        elif s < 4096 then 30, 11
        elif s < 8192 then 31, 12
        elif s < 16384 then 32, 13
        elif s < 32768 then 33, 14
        elif s < 65536 then 34, 15
        else 35, 16
        
    let encodeOff s =
        if s < 4 then s, 0
        elif (s < 4+8192) then 4 + (s-4)/512, 9  //4-19
        elif (s < 4+8192+16384) then 20 + (s-4-4096)/1024, 10 //20-35
        elif (s < 4+8192+16384+65536) then 36 + (s-4-4096-65536)/4096, 12 //36-51
        else 52 + s/16384, 14
        
    let encodeOff s =
        let code =
            if s = 0 then 0                
            elif s < 4 then 1 //3 = 2
            elif s < 8 then 2
            elif s < 16 then 3
            elif s < 32 then 4
            elif s < 64 then 5
            elif s < 128 then 6
            elif s < 256 then 7
            elif s < 512 then 8
            elif s < 1024 then 9
            elif s < 2048 then 10
            elif s < 4096 then 11
            elif s < 8192 then 12
            elif s < 16384 then 13
            elif s < 32768 then 14
            elif s < 65536 then 15
            elif s < 65536*2 then 16
            else 17
        code, code        
        
    //8192
    let encodeOff s =  
        if s < 4 then s, 0
        else (s-4)/16 + 4, 4
        //elif s < 256+4 then (s-4)/16 + 4, 4 //512/
        //else (s-256-4)/32 + 20, 5
//        if s < 256 then s/16, 4        
//        else (s-256)/256 + 16, 8           
//        if s < 4 then s, 0
//        elif (s < 4 + 1024) then 4 + (s-4)/64, 6 
//        elif (s < 4 + 1024 + 4096) then 20 + (s-4-1024)/256, 8        
//        else 36 + (s-4-1024-4096)/1024, 10 //20-35
    
    let encodeSeq (input: ResizeArray<int>) =
        
        offH.Clear()
        
        let count = input.Count/3
        let codes = Array.zeroCreate count
        let offs = Array.zeroCreate count
        let offs2 = Array.zeroCreate count 
        let lengths = Array.zeroCreate count 
        let mutable ls = 0
        let mutable os = 0
        for i = 0 to count-1 do
//            let s = input.[i*3+1]
//            let code =
//                if s = 0 then 0                
//                elif s < 4 then 1 //3 = 2
//                elif s < 8 then 2
//                elif s < 16 then 3
//                elif s < 32 then 4
//                elif s < 64 then 5
//                elif s < 128 then 6
//                elif s < 256 then 7
//                elif s < 512 then 8
//                elif s < 1024 then 9
//                elif s < 2048 then 10
//                elif s < 4096 then 11
//                elif s < 8192 then 12
//                elif s < 16384 then 13
//                elif s < 32768 then 14
//                elif s < 65536 then 15
//                elif s < 65536*2 then 16
//                else 17
//            let more = 17             
            //let s = input.[i*3+1]
            
                
            
//            let code, more =
//                if s < 35 then s, 0
//                elif s < 37 then 32, 1
//                elif s < 39 then 33, 1
//                elif s < 41 then 34, 1
//                elif s < 43 then 35, 1
//                elif s < 47 then 36, 2
//                elif s < 51 then 37, 2
//                elif s < 59 then 38, 3
//                elif s < 67 then 39, 3
//                elif s < 83 then 40, 4
//                elif s < 99 then 41, 4
//                elif s < 131 then 42, 5
//                elif s < 259 then 43, 7
//                elif s < 515 then 44, 8
//                elif s < 1027 then 45, 9
//                elif s < 2051 then 46, 10
//                elif s < 4099 then 47, 11
//                elif s < 8195 then 48, 12
//                elif s < 16387 then 49, 13
//                elif s < 32771 then 50, 14
//                elif s < 65539 then 51, 15
//                else 52, 16
            let code, more = encodeLit input.[i*3+0]
            codes.[i] <- byte code
            os <- os + more
            
            let off = input.[i*3+1]
//            let off =
//                let mutable i = 0
//                let mutable r = off + 4
//                for x in offH do
//                    if x = off && r = off + 4 then r <- i
//                    i <- i + 1                
//                if offH.Count >= 4 then  offH.Dequeue() |> ignore
//                offH.Enqueue off            
//                r
            let orgOff = off 
            let off =                 
                let mutable i = 0
                let mutable m = off/256 
                let mutable r = m + 4                
                let mutable node = offH.First
                while not (isNull node) && r >= 4 do
                    if node.Value = m then
                        r <- i
                        if node <> offH.First then
                            offH.Remove node
                            offH.AddFirst node
                    node <- node.Next
                    i <- i + 1 
                if r >= 4 then
                    if offH.Count >= 4 then offH.RemoveLast()
                    offH.AddFirst (LinkedListNode(off))                
                r
            
            let code, more = encodeOff off
            offs.[i] <- byte code
            offs2.[i] <- orgOff%256 
            ls <- ls + more
            let code, more = encodeLit input.[i*3+2]
            lengths.[i] <- byte code            
            os <- os + more      
            
        let dest = Array.zeroCreate<byte> (count*2)
        //let lengthCompress = Fse.huf_compress codes 0 dest 0 count
        use srcPC = fixed(&codes.[0])
        use srcPO = fixed(&offs.[0])
        use srcPN = fixed(&lengths.[0])
        use dstP = fixed(&dest.[0])
        let srcPO2 = fixed(&offs2.[0])
        for i = 10 to 12 do
            let mutable lc = 0
            //let lengthCompress = int <| Fse.HUF_compress2 (dstP |> NativePtr.toNativeInt, int64 <| count*2, srcP |> NativePtr.toNativeInt, int64 count, uint32 m, uint32 i)
            let mc = Seq.max codes
            let mo = Seq.max offs
            let mo2 = Seq.max offs2
            let mn = Seq.max lengths
            let mutable to2 = 0 
            for j = 0 to (count+Fse.HufBlock-1)/Fse.HufBlock - 1 do
                let srcC = NativePtr.add srcPC (j*Fse.HufBlock)
                let srcO = NativePtr.add srcPO (j*Fse.HufBlock)
                let srcO2 = NativePtr.add srcPO2 (j*Fse.HufBlock)
                let srcN = NativePtr.add srcPN (j*Fse.HufBlock)
                let size = min Fse.HufBlock (count - j * Fse.HufBlock)
                let t = int <| Fse.FSE_compress2 (dstP |> NativePtr.toNativeInt, int64 <| count*2, srcC |> NativePtr.toNativeInt, int64 size, uint32 mc, uint32 i)
                lc <- lc + t
                let t = int <| Fse.FSE_compress2 (dstP |> NativePtr.toNativeInt, int64 <| count*2, srcN |> NativePtr.toNativeInt, int64 size, uint32 mn, uint32 i)
                lc <- lc + t
                let t = int <| Fse.FSE_compress2 (dstP |> NativePtr.toNativeInt, int64 <| count*2, srcO |> NativePtr.toNativeInt, int64 size, uint32 mo, uint32 i)
                lc <- lc + t
                let t = int <| Fse.FSE_compress2 (dstP |> NativePtr.toNativeInt, int64 <| count*2, srcO2 |> NativePtr.toNativeInt, int64 size, uint32 mo2, uint32 (i-3))
                lc <- lc + t
                to2 <- to2 + t 
            printfn "length: %i code: %i  ls: %i  os: %i/ o2: %i  all:%i" count lc (ls/8) (os/8) to2 (lc + ls/8 + os/8) 
        
    
    let output = Array.zeroCreate bytes.Length
    
    let decompression () =
        let mutable pos = 0
        let mutable lpos = 0 
        for i = 0 to ts.Count/3-1 do
            let l = ts.[i*3+0]
            let offset = ts.[i*3+1]
            let n = ts.[i*3+2]
            if l > 0 then
                Array.Copy(literalBuf, lpos, output, pos, l)
                lpos <- lpos + l
                pos  <- pos + l
            if n > 0 then
                //Array.Copy(output, pos - offset, output, pos, n)
                for i = 0 to n-1 do
                    output.[pos + i] <- output.[pos - offset + i]
                pos <- pos + n    
    
    decompression ()        
    let w = Stopwatch.StartNew()
    decompression ()
    w.Stop()
    printfn "def-decompression take: %i ms" w.ElapsedMilliseconds
    
    let mutable f = 0
    for i = 0 to bytes.Length-1 do
        if bytes.[i] <> output.[i] then f <- f + 1
        
    let hufOutput = Array.zeroCreate (l*2)
    
    let w = Stopwatch.StartNew()
    
    let hufS = Fse.huf_compress literalBuf 0 hufOutput 0 l
    w.Stop()
    
    let offs = Array.init (ts.Count/3) (fun i -> byte (ts.[i]%16))
    let mls = Array.init (ts.Count/3) (fun i -> (ts.[i*3+2]))
    let freq = Array.zeroCreate (1 + int (Seq.max offs))
    histogram offs offs.Length freq
    for i = 0 to freq.Length-1 do
        printfn "%3i - %i - %.2f" i freq.[i] (float32 freq.[i] * 100.0f / float32 offs.Length) 
    
    let hufDeOutput = Array.zeroCreate (literalBuf.Length*2)
    let w = Stopwatch.StartNew()    
    let hufDS = Fse.huf_decompress hufOutput 0 hufDeOutput 0 0 
    w.Stop()
    let mutable hf = 0
    for i = 0 to min literalBuf.Length hufDS - 1 do
        if hufDeOutput.[i] <> literalBuf.[i] then
            hf <- hf + 1 
    printfn "huf        : %i = %i/%i = 0" literalBuf.Length hufDS hf 
    printfn "huf-de take: %i ms" w.ElapsedMilliseconds
    printfn "min length : %i" (mls |> Seq.min)
        
    printfn "failed     : %i" f         
    printfn "org        : %i bytes" bytes.Length
    printfn "fs         : %i bytes" fs
    printfn "literal    : %i bytes" l
    printfn "gz literal : %i bytes" lmem.Length
    printfn "huf-l      : %i bytes" hufS
    l <- min (int lmem.Length) hufS
    printfn "seqs       : %i bytes/ raw: %i bytes - %i %%" (s/8) (ts.Count/3*4) (int64 s/8L*100L/int64 (ts.Count/3*4))
    encodeSeq ts 
    printfn "copy       : %i bytes" cp
    let cs = l + s/8    
    printfn "ratio      : %.2f " (float32 cs * 100.0f / float32 bytes.Length)
    printfn "size       : %i KB" ((l+s/8)/1024)
    0 // return an integer exit code
