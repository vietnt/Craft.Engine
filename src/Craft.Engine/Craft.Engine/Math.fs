module Craft.Engine.Math

#nowarn "9"

open System
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86


[<Struct>]
[<StructLayout(LayoutKind.Explicit, Size = 16)>]
type Vector4 = {
    [<FieldOffset(00)>] mutable mm    : Vector128<float32>        
}
with
    member inline this.x with get() = this.mm.GetElement(0)
    member inline this.y with get() = this.mm.GetElement(1)
    member inline this.z with get() = this.mm.GetElement(2)
    member inline this.w with get() = this.mm.GetElement(3)
    
[<Struct>]
[<StructLayout(LayoutKind.Sequential, Size = 12)>]
type Vector3 = {
    mutable x     : float32
    mutable y     : float32
    mutable z     : float32        
}

[<Struct>]
[<StructLayout(LayoutKind.Explicit, Size = 8)>]
type Vector2 = {
    [<FieldOffset(0)>]mutable x     : float32
    [<FieldOffset(4)>]mutable y     : float32
}

let inline to_vector4 (v: Vector128<float32>) = { mm = v; }                

[<Struct>]
[<StructLayout(LayoutKind.Explicit, Size = 64)>]
type Matrix4x4 = {
    [<FieldOffset(00)>] mutable c0    : Vector4
    [<FieldOffset(16)>] mutable c1    : Vector4
    [<FieldOffset(32)>] mutable c2    : Vector4
    [<FieldOffset(48)>] mutable c3    : Vector4
}
with
    member x.getRow i =
        let c0 = x.c0.mm
        let c1 = x.c1.mm
        let c2 = x.c2.mm
        let c3 = x.c3.mm
        Vector128.Create (c0.GetElement i, c1.GetElement i, c2.GetElement i, c3.GetElement i) |> to_vector4 

let inline shuffle (a: Vector4) (b: Vector4) c = Sse41.Shuffle(a.mm, b.mm, c) |> to_vector4
let inline mul (x: Vector4) (y: Vector4) = Sse41.Multiply(x.mm, y.mm) |> to_vector4

type Vector4 with
    static member (*) (x: Vector4, y: Vector4) = mul x y
    static member (*) (x: Vector4, y: float32) = Sse41.Multiply (x.mm, (Vector128.Create (y, y, y, y))) |> to_vector4
    static member (+) (x: Vector4, y: Vector4) = Sse41.Add(x.mm, y.mm) |> to_vector4
    static member (-) (x: Vector4, y: Vector4) = Sse41.Subtract (x.mm, y.mm) |> to_vector4
    static member (~-) (x: Vector4) = Sse41.Subtract(Vector128.Zero, x.mm) |> to_vector4
    member inline x.xxxx with get() = shuffle x x 0b00_00_00_00uy
    member inline x.yyyy with get() = shuffle x x 0b01_01_01_01uy
    member inline x.zzzz with get() = shuffle x x 0b10_10_10_10uy
    member inline x.wwww with get() = shuffle x x 0b11_11_11_11uy    
    
let inline mulV (v: Vector4) (m: Matrix4x4) =                        
    let t0 = m.c0 * (shuffle v v 0b00000000uy)
    let t1 = m.c1 * (shuffle v v 0b01010101uy)
    let t2 = m.c2 * (shuffle v v 0b10101010uy)
    let t3 = m.c3 * (shuffle v v 0b11111111uy)
    (t0 + t1) + (t2 + t3)
    
let inline move_hdup (m: Vector4) = Sse41.MoveHighAndDuplicate m.mm |> to_vector4   
let inline move_hl (a: Vector4) (b: Vector4) = Sse41.MoveHighToLow(a.mm, b.mm) |> to_vector4
let inline unpack_lo (a: Vector4) (b: Vector4) = Sse42.UnpackLow (a.mm, b.mm) |> to_vector4
let inline unpack_hi (a: Vector4) (b: Vector4) = Sse42.UnpackHigh (a.mm, b.mm) |> to_vector4

let inline _select (a: Vector128<float32>) b mask =    
    let y = Sse41.AndNot(mask, a)
    let x = Sse41.And(mask, b)
    Sse41.Or(x, y)
       
let inline select (a: Vector4) (b: Vector4) (mask: Vector4) = _select a.mm b.mm mask.mm |> to_vector4        

let inline toScalar (a: Vector4) = a.mm.ToScalar()

let inline vec3 x y z : Vector3 = { x = x; y = y; z = z }
let inline vec2 x y : Vector2 = { x = x; y = y }    
let inline vec4 (x: float32) y z w =  Vector128.Create(x, y, z, w) |> to_vector4
let inline vec43 (v: Vector3) w = vec4 v.x v.y v.z w
let inline vec34 (v: Vector4) = vec3 v.x v.y v.z

let inline dot (a: Vector4) (b: Vector4) =
    let m = a * b 
    let sh = move_hdup m
    let sum = sh + m 
    let t = move_hl sh sum
    sum + t |> toScalar
    
let inline cross (a: Vector4) (b: Vector4) =
    let t0 = shuffle a a 0b11_00_10_01uy
    let t1 = shuffle b b 0b11_01_00_10uy
    let t2 = shuffle a a 0b11_01_00_10uy
    let t3 = shuffle b b 0b11_00_10_01uy
    t2*t3 - t0 * t1
    

type Vector4 with
    static member normalize (a: Vector4) = a * (1.0f / MathF.Sqrt(dot a a))
    
    static member rotate y p r =
        let halfR = r * 0.5f
        let sr = MathF.Sin halfR
        let cr = MathF.Cos halfR
        let halfP = p * 0.5f
        let sp = MathF.Sin halfP
        let cp = MathF.Cos halfP
        let halfY = y * 0.5f
        let sy = MathF.Sin halfY
        let cy = MathF.Cos halfY
        //
        let x = cy * sp * cr + sy * cp * sr;
        let y = sy * cp * cr - cy * sp * sr;
        let z = cy * cp * sr - sy * sp * cr;
        let w  = cy * cp * cr + sy * sp * sr
        //
        vec4 x y z w 

type Vector3 with
    static member (~-) (v: Vector3) = vec3 -v.x -v.y -v.z
    static member (-) (a: Vector3, b: Vector3) = vec3 (a.x - b.x) (a.y - b.y) (a.z - b.z)
    static member (+) (a: Vector3, b: Vector3) = vec3 (a.x + b.x) (a.y + b.y) (a.z + b.z)
    static member (*) (a: Vector3, b: float32) = vec3 (a.x * b) (a.y * b) (a.z * b)    
    
type Vector2 with        
    static member (+) (a: Vector2, b: Vector2) = vec2 (a.x + b.x) (a.y + b.y)
    static member (*) (a: Vector2, b: float32) = vec2 (a.x * b) (a.y * b)

let inline mat4 col0 col1 col2 col3 = { c0 = col0; c1 = col1; c2 = col2; c3 = col3 }                    

    
module Vector2 =
    
    let zero = vec2 0.f 0.f
    
module Vector3 =
    
    let zero = vec3 0.f 0.f 0.f
    
    let inline dot (a: Vector3) (b: Vector3) = a.x * b.x + a.y * b.y + a.z * b.z 
    
    let inline normalize (v: Vector3) =
        let s = MathF.Sqrt(dot v v)
        vec3 (v.x/s) (v.y/s) (v.z/s)
        
    let inline lengthSqr (a: Vector3) = dot a a
    
    let inline length (a: Vector3) = lengthSqr a |> MathF.Sqrt
    
    let inline cross a b = cross (vec43 a 0.f) (vec43 b 0.f) |> vec34

module Matrix4x4 =
    
    let identity = mat4 (vec4 1.0f 0.f 0.f 0.f) (vec4 0.f 1.0f 0.f 0.f) (vec4 0.f 0.f 1.0f 0.f) (vec4 0.f 0.f 0.f 1.f)
    
    let inline createMatrixTranspose m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44 =
        mat4 (vec4 m11 m21 m31 m41) (vec4 m12 m22 m32 m42) (vec4 m13 m23 m33 m43) (vec4 m14 m24 m34 m44)
    
    let inline mulV (v: Vector4) (m: Matrix4x4) =                        
        let t0 = m.c0 * (shuffle v v 0b00000000uy)
        let t1 = m.c1 * (shuffle v v 0b01010101uy)
        let t2 = m.c2 * (shuffle v v 0b10101010uy)
        let t3 = m.c3 * (shuffle v v 0b11111111uy)
        (t0 + t1) + (t2 + t3)
                
    let inline mulV3 (v: Vector3) (m: Matrix4x4) =
        let v = vec43 v 0.f
        let t0 = m.c0 * (shuffle v v 0b00000000uy)
        let t1 = m.c1 * (shuffle v v 0b01010101uy)
        let t2 = m.c2 * (shuffle v v 0b10101010uy)            
        t0 + t1 + t2
        |> vec34
        
    let inline mul (a: Matrix4x4) (b: Matrix4x4) =
        mat4 (mulV b.c0 a) (mulV b.c1 a) (mulV b.c2 a) (mulV b.c3 a)        
        
    let rotationXY rX rY =
        let cosX = MathF.Cos rX
        let sinX = MathF.Sin rX
        let cosY = MathF.Cos rY
        let sinY = MathF.Sin rY 
        
        createMatrixTranspose
            cosY 0.f sinY 0.f
            (sinX * sinY) cosX (-sinX * cosY) 0.f
            (cosX * -sinY) sinX (cosX * cosY) 0.f
            0.f 0.f 0.f 1.f
            
    let rotationYX rY rX =
        let cosX = MathF.Cos rX
        let sinX = MathF.Sin rX
        let cosY = MathF.Cos rY
        let sinY = MathF.Sin rY 
        
        createMatrixTranspose
            cosY (sinY * sinY) (sinY * cosX) 0.f
            0.f cosX -sinX 0.f
            -sinY (cosY * sinX) (cosY * cosX) 0.f
            0.f 0.f 0.f 1.f
            
    let fromQuaternion (q: Vector4) =
        let selectX = (Vector128.Create (0xFFFFFFFF, 0, 0, 0)).AsSingle() |> to_vector4
        let selectZ = (Vector128.Create (0, 0, 0xFFFFFFFF, 0)).AsSingle() |> to_vector4
        let selectW = (Vector128.Create (0, 0, 0, 0xFFFFFFFF)).AsSingle() |> to_vector4
        
        let xyzw2 = q + q
        let wwww = q.wwww
        
        let yzxw = shuffle q q 0b11_00_10_01uy
        let zxyw = shuffle q q 0b11_01_00_10uy
        let yzxw2 = shuffle xyzw2 xyzw2 0b11_00_10_01uy
        let zxyw2 = shuffle xyzw2 xyzw2 0b11_01_00_10uy
        
        let tmp0 = yzxw2 * wwww
        let tmp1 = to_vector4 (Vector128.CreateScalar 1.0f) - yzxw * yzxw2
        let tmp2 = yzxw * xyzw2
        
        let tmp0 = zxyw * xyzw2 + tmp0
        let tmp1 = tmp1 - zxyw * zxyw2
        let tmp2 = tmp2 - zxyw2 * wwww
        let tmp3 = select tmp0 tmp1 selectX
        let tmp4 = select tmp1 tmp2 selectX
        let tmp5 = select tmp2 tmp0 selectX
        let zero = Vector128.Zero |> to_vector4
        
        let c0 = select (select tmp3 tmp2 selectZ) zero selectW 
        let c1 = select (select tmp4 tmp0 selectZ) zero selectW
        let c2 = select (select tmp5 tmp1 selectZ) zero selectW
        {
            c0 = c0
            c1 = c1
            c2 = c2
            c3 = zero
        }
            
    let inline setTranslation (v: Vector4) (m: Matrix4x4) =
        { m with c3 = vec4 v.x v.y v.z m.c3.w }                        
        
    let translation (v: Vector3) = { identity with c3 = vec43 v 1.0f}
    
    let rotateEuler (v: Vector3) =
        Vector4.rotate v.x v.y v.z
        |> fromQuaternion 
    
    let inline transpose (m: Matrix4x4) =
        let t0 = unpack_lo m.c0 m.c2
        let t1 = unpack_lo m.c1 m.c3
        let t2 = unpack_hi m.c0 m.c2
        let t3 = unpack_hi m.c1 m.c3
        let r0 = unpack_lo t0 t1
        let r1 = unpack_hi t0 t1
        let r2 = unpack_lo t2 t3
        let r3 = unpack_hi t2 t3
        { c0 = r0; c1 = r1; c2 = r2; c3 = r3 }
        
    let orthoInverse (m: Matrix4x4) =
        
        let inline mergeH a b = unpack_lo a b
        let inline mergeL a b = unpack_hi a b 
        
        let tmp0 = mergeH m.c0 m.c2
        let tmp1 = mergeL m.c0 m.c2
        let inv3 = -m.c3
        let inv0 = mergeH tmp0 m.c1
        
        
        let selectY = Vector128.Create(0, 0xFFFFFFFF, 0, 0).AsSingle()
        
        let inv1 = shuffle tmp0 tmp0 0b00_11_10_10uy
        let inv1 = _select inv1.mm m.c1.mm selectY |> to_vector4
        //let inv1 = vec4 inv1.x m.c1.y inv1.z inv1.w
        
        let inv2 = shuffle tmp1 tmp1 0b00_01_01_00uy
        //let inv2 = vec4 inv2.x m.c1.z inv2.z inv2.w
        let inv2 = _select inv2.mm m.c1.zzzz.mm selectY |> to_vector4
        
//        let yyy = vec4 inv3.y inv3.y inv3.y inv3.y
//        let zzz = vec4 inv3.z inv3.z inv3.z inv3.z
//        let xxx = vec4 inv3.x inv3.x inv3.x inv3.x
        
        let inv3 = inv0 * inv3.xxxx + inv1 * inv3.yyyy + inv2 * inv3.zzzz
        
        mat4 (vec4 inv0.x inv0.y inv0.z 0.f) (vec4 inv1.x inv1.y inv1.z 0.f) (vec4 inv2.x inv2.y inv2.z 0.f) (vec4 inv3.x inv3.y inv3.z 1.f)
        
        
        
    let createPFOV fov aspect znear zfar =
//        let f = fov * 0.5f
//        let sin = MathF.Sin f
//        let cos = MathF.Cos f
//        let h = cos / sin
//        let w = h / aspect
//        let fRange = zfar / (zfar - znear)
//        
//        createMatrixTranspose
//            w 0.f 0.f 0.f
//            0.f h 0.f 0.f
//            0.f 0.f fRange 1.f
//            0.f 0.f (-fRange*znear) 0.f            
//        |> transpose
        
        let aspect = 1.0f / aspect
        //let fov = fov * aspect
        
        let f = MathF.Tan (MathF.PI*0.5f - fov*0.5f)
        let rangeInv = 1.0f / (zfar - znear)
        let c0 = vec4 f 0.f 0.f 0.f
        let c1 = vec4 0.f (f/aspect) 0.f 0.f
        let c2 = vec4 0.f 0.f (zfar * rangeInv) 1.0f
        let c3 = vec4 0.f 0.f (-znear * zfar * rangeInv) 0.f
        mat4 c0 c1 c2 c3
        
//        let n1 = 1.0f / MathF.Tan(fov*0.5f)
//        let n2 = zfar / (zfar - znear)
//        mat4 (vec4 (n1/aspect) 0.f 0.f 0.f) (vec4 0.f n1 0.f 0.f) (vec4 0.f 0.f n2 0.f) (vec4 0.f 0.f (-n2*znear) 0.f)
        
        //m.M11 <- n1 / aspect
        //m.M22 <- n1
        //m.M33 <- n2
        //m.M34 <- 1.f
        //m.M43 <- -n2 * znear
        //m
        
    let createLookAt (eye: Vector3) (target: Vector3) (up: Vector3) =
        
        let v3y = up
        let v3z = eye - target |> Vector3.normalize
        let v3x = Vector3.cross v3y v3z |> Vector3.normalize
        let v3y = Vector3.cross v3z v3x
        mat4 (vec43 v3x 0.f) (vec43 v3y 0.f) (vec43 v3z 0.f) (vec43 eye 0.f)
        |> orthoInverse
        
    let extractFrustumClipPlanes (m: Matrix4x4) =
        let r0 = m.getRow 0
        let r1 = m.getRow 1
        let r2 = m.getRow 2 
        let r3 = m.getRow 3
        let left = r3 + r0 |> Vector4.normalize
        let right = r3 - r0 |> Vector4.normalize
        let bottom = r3 + r1 |> Vector4.normalize
        let top = r3 - r1 |> Vector4.normalize
        let near = r3 + r2 |> Vector4.normalize
        let far = r3 - r2 |> Vector4.normalize
        
        [| left; right; top; bottom; far; near |]
        
    let inline inverse (m: Matrix4x4) =
        let m = System.Numerics.Matrix4x4(m.c0.x, m.c0.y, m.c0.z, m.c0.w, m.c1.x, m.c1.y, m.c1.z, m.c1.w, m.c2.x, m.c2.y, m.c2.z, m.c2.w, m.c3.x, m.c3.y, m.c3.z, m.c3.w)        
        let _, m2 = System.Numerics.Matrix4x4.Invert(m)
        {
            c0 = vec4 m2.M11 m2.M12 m2.M13 m2.M14
            c1 = vec4 m2.M21 m2.M22 m2.M23 m2.M24
            c2 = vec4 m2.M31 m2.M32 m2.M33 m2.M34
            c3 = vec4 m2.M41 m2.M42 m2.M43 m2.M44
        }
        
    let inline createOrtho left right bot top nz fz =
        let rw = 1.0f / (right - left)
        let rh = 1.0f / (top - bot)
        let fR = 1.0f / (fz - nz)
        {
            c0 = vec4 (rw + rh) 0.f 0.f 0.f
            c1 = vec4 0.f (rw + rh) 0.f 0.f
            c2 = vec4 0.f 0.f fR 0.f
            c3 = vec4 (-(left + right) * rw) (-(top+bot)*rh) (-fR * nz) 1.0f             
        }
        
    let createOrtho2 w h nz fz =
        let fR = 1.0f / (fz - nz)
        {
            c0 = vec4 (2.0f/w) 0.f 0.f 0.0f
            c1 = vec4 0.f (-2.0f/h) 0.f 0.0f
            c2 = vec4 0.f 0.f fR 0.f
            c3 = vec4 -1.0f 1.f (-fR * nz) 1.0f             
        }
        
        
type Matrix4x4 with
    static member (*) (a: Matrix4x4, b: Vector4) = a |> Matrix4x4.mulV b
    static member (*) (a: Matrix4x4, b: Vector3) = a |> Matrix4x4.mulV3 b
    static member (*) (a: Matrix4x4, b: Matrix4x4) = Matrix4x4.mul a b 

let sseCull (spheres: Temp.NativeArray<Vector4>) offset count (result: Temp.NativeArray<int>) (planes: Vector4[]) =        
    let plane = [|
            Vector128.Create (-planes.[0].x, -planes.[1].x, -planes.[2].x, -planes.[3].x)
            Vector128.Create (-planes.[0].y, -planes.[1].y, -planes.[2].y, -planes.[3].y)
            Vector128.Create (-planes.[0].z, -planes.[1].z, -planes.[2].z, -planes.[3].z)
            Vector128.Create (-planes.[0].w, -planes.[1].w, -planes.[2].w, -planes.[3].w)
            
            Vector128.Create (-planes.[4].x, -planes.[5].x, -planes.[4].x, -planes.[5].x)
            Vector128.Create (-planes.[4].y, -planes.[5].y, -planes.[4].y, -planes.[5].y)
            Vector128.Create (-planes.[4].z, -planes.[5].z, -planes.[4].z, -planes.[5].z)
            Vector128.Create (-planes.[4].w, -planes.[5].w, -planes.[4].w, -planes.[5].w)
        |]
    
    let ptr = spheres.Ptr |> NativeInterop.NativePtr.ofNativeInt<float32>
    
    for j = 0 to count-1 do
        let i = offset + j 
        let s = Sse42.LoadVector128 (NativeInterop.NativePtr.add ptr (i*4))
        let x = Sse42.Shuffle(s, s, 0uy)
        let y = Sse42.Shuffle(s, s, 0b01010101uy)
        let z = Sse42.Shuffle(s, s, 0b10101010uy)
        let w = Sse42.Shuffle(s, s, 0b11111111uy)
        
        let mutable v = Sse42.Add (Sse42.Multiply(x, plane.[0]), plane.[3])
        v <- Sse42.Add (Sse42.Multiply(y, plane.[1]), v)
        v <- Sse42.Add (Sse42.Multiply(z, plane.[2]), v)
        let mutable r = Sse42.CompareGreaterThan(v, w)
        
        v <- Sse42.Add (Sse42.Multiply(x, plane.[4]), plane.[7])
        v <- Sse42.Add (Sse42.Multiply(y, plane.[5]), v)
        v <- Sse42.Add (Sse42.Multiply(z, plane.[6]), v)
        r <- Sse42.Or (r, Sse42.CompareGreaterThan(v, w))
        
        r <- Sse42.Or(r, Sse42.MoveHighToLow(r, r))
        r <- Sse42.Or(r, Sse42.Shuffle(r, r, 0b01010101uy))           
        //result.[i] <- r.AsInt32().GetElement(0) |> int32 &&& 1
        result |> Temp.write i (r.AsInt32().GetElement(0))                    