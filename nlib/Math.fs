module Nlib.Math

#nowarn "193"

open FsAlg.Generic
open System

// Random math funcs
type Vec2 = struct
    val mutable x: float32
    val mutable y: float32
end with
    new(x', y') = { x = x'; y = y' }

    static member inline (+) (a: Vec2, b: Vec2): Vec2 = Vec2( a.x + b.x, a.y + b.y )
    static member inline (+) (a: Vec2, b: float32): Vec2 = Vec2( a.x + b, a.y + b )
    static member inline (+) (a: float32, b: Vec2): Vec2 = Vec2( a + b.x, a + b.y )

    static member inline (~-) (a: Vec2): Vec2 = Vec2( -a.x, -a.y )

    static member inline (-) (a: Vec2, b: Vec2): Vec2 = Vec2( a.x - b.x, a.y - b.y )
    static member inline (-) (a: Vec2, b: float32): Vec2 = Vec2( a.x - b, a.y - b )
    static member inline (-) (a: float32, b: Vec2): Vec2 = Vec2( a - b.x, a - b.y )

    static member inline (*) (a: Vec2, b: Vec2): Vec2 = Vec2( a.x * b.x, a.y * b.y )
    static member inline (*) (a: Vec2, b: float32): Vec2 = Vec2( a.x * b, a.y * b )
    static member inline (*) (a: float32, b: Vec2): Vec2 = Vec2( a * b.x, a * b.y )

    static member inline (/) (a: Vec2, b: Vec2): Vec2 = Vec2( a.x / b.x, a.y / b.y )
    static member inline (/) (a: Vec2, b: float32): Vec2 = Vec2( a.x / b, a.y / b )
    static member inline (/) (a: float32, b: Vec2): Vec2 = Vec2( a / b.x, a / b.y )

    static member inline ( *.) (a: Vec2, b: Vec2) = a.x * b.x + a.y * b.y

    static member inline ofArray (a: float32 []) = Vec2( a.[0], a.[1] )

    member inline v.toArray() = [| v.x; v.y |]
    member inline v.SqLen = v *. v
    member inline v.Len = sqrt <| v.x * v.x + v.y * v.y
    member inline v.Normal = v / v.Len
    member inline v.Perp = Vec2( -v.y, v.x )

let inline vec2 x y = Vec2( x, y )

type Vec3 = struct
    val mutable x: float32
    val mutable y: float32
    val mutable z: float32
end with
    new(x', y', z') = { x = x'; y = y'; z = z' }

    static member inline (+) (a: Vec3, b: Vec3): Vec3 = Vec3( a.x + b.x, a.y + b.y, a.z + b.z )
    static member inline (+) (a: Vec3, b: float32): Vec3 = Vec3( a.x + b, a.y + b, a.z + b )
    static member inline (+) (a: float32, b: Vec3): Vec3 = Vec3( a + b.x, a + b.y, a + b.z )

    static member inline (~-) (a: Vec3): Vec3 = Vec3( -a.x, -a.y, -a.z )

    static member inline (-) (a: Vec3, b: Vec3): Vec3 = Vec3( a.x - b.x, a.y - b.y, a.z - b.z )
    static member inline (-) (a: Vec3, b: float32): Vec3 = Vec3( a.x - b, a.y - b, a.z - b )
    static member inline (-) (a: float32, b: Vec3): Vec3 = Vec3( a - b.x, a - b.y, a - b.z )

    static member inline (*) (a: Vec3, b: Vec3): Vec3 = Vec3( a.x * b.x, a.y * b.y, a.z * b.z )
    static member inline (*) (a: Vec3, b: float32): Vec3 = Vec3( a.x * b, a.y * b, a.z * b )
    static member inline (*) (a: float32, b: Vec3): Vec3 = Vec3( a * b.x, a * b.y, a * b.z )

    static member inline (/) (a: Vec3, b: Vec3): Vec3 = Vec3( a.x / b.x, a.y / b.y, a.z / b.z )
    static member inline (/) (a: Vec3, b: float32): Vec3 = Vec3( a.x / b, a.y / b, a.z / b )
    static member inline (/) (a: float32, b: Vec3): Vec3 = Vec3( a / b.x, a / b.y, a / b.z )

    static member inline ( *.) (a: Vec3, b: Vec3): 'a = a.x * b.x + a.y * b.y + a.z * b.z

    static member inline ( %* ) (a: Vec3, b: Vec3) =
        Vec3( a.y * b.z - a.z * b.y, a.x * b.z - a.z * b.x, a.x * b.y - a.y * b.x )

    static member inline ofArray (a: float32 []) = Vec3( a.[0], a.[1], a.[2] )

    member inline v.toArray() = [| v.x; v.y; v.z |]
    member inline v.SqLen = v *. v
    member inline v.Len = sqrt <| v.x * v.x + v.y * v.y + v.z * v.z
    member inline v.Normal = v / v.Len

let vec3 x y z = Vec3( x, y, z )

type Vec4 = struct
    val mutable x: float32
    val mutable y: float32
    val mutable z: float32
    val mutable w: float32
end with
    new(x', y', z', w') = { x = x'; y = y'; z = z'; w = w' }

    static member inline (+) (a: Vec4, b: Vec4): Vec4 = Vec4( a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w )
    static member inline (+) (a: Vec4, b: float32): Vec4 = Vec4( a.x + b, a.y + b, a.z + b, a.w + b )
    static member inline (+) (a: float32, b: Vec4): Vec4 = Vec4( a + b.x, a + b.y, a + b.z , a + b.w )

    static member inline (~-) (a: Vec4): Vec4 = Vec4( -a.x, -a.y, -a.z, -a.w )

    static member inline (-) (a: Vec4, b: Vec4): Vec4 = Vec4( a.x - b.x, a.y - b.y, a.z - b.z, a.w + b.w )
    static member inline (-) (a: Vec4, b: float32): Vec4 = Vec4( a.x - b, a.y - b, a.z - b, a.w - b )
    static member inline (-) (a: float32, b: Vec4): Vec4 = Vec4( a - b.x, a - b.y, a - b.z, a - b.w )

    static member inline (*) (a: Vec4, b: Vec4): Vec4 = Vec4( a.x * b.x, a.y * b.y, a.z * b.z, a.w + b.w )
    static member inline (*) (a: Vec4, b: float32): Vec4 = Vec4( a.x * b, a.y * b, a.z * b, a.w * b )
    static member inline (*) (a: float32, b: Vec4): Vec4 = Vec4( a * b.x, a * b.y, a * b.z, a * b.w )

    static member inline (/) (a: Vec4, b: Vec4): Vec4 = Vec4( a.x / b.x, a.y / b.y, a.z / b.z, a.w + b.w )
    static member inline (/) (a: Vec4, b: float32): Vec4 = Vec4( a.x / b, a.y / b, a.z / b, a.w / b )
    static member inline (/) (a: float32, b: Vec4): Vec4 = Vec4( a / b.x, a / b.y, a / b.z, a / b.w )

    static member inline ( *.) (a: Vec4, b: Vec4): 'a = a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w

    static member inline ofArray (a: float32 []) = Vec4( a.[0], a.[1], a.[2], a.[3] )

    member inline v.toArray() = [| v.x; v.y; v.z; v.w |]
    member inline v.SqLen = v *. v
    member inline v.Len = sqrt <| v.SqLen
    member inline v.Normal = v / v.Len

let vec4 x y z w = Vec4( x, y, z, w )

type Mat3x3 = struct
    val mutable m00: float32; val mutable m01: float32; val mutable m02: float32
    val mutable m10: float32; val mutable m11: float32; val mutable m12: float32
    val mutable m20: float32; val mutable m21: float32; val mutable m22: float32
end with
    new( a: float32 [] ) = {
        m00 = a.[0]; m01 = a.[1]; m02 = a.[2];
        m10 = a.[3]; m11 = a.[4]; m12 = a.[5];
        m20 = a.[6]; m21 = a.[7]; m22 = a.[8];
    }
    new( x: Vec3, y: Vec3, z: Vec3 ) = {
        m00 = x.x; m01 = x.y; m02 = x.z;
        m10 = y.x; m11 = y.y; m12 = y.z;
        m20 = z.x; m21 = z.y; m22 = z.z;
    }

type Mat4x4 = struct
    val mutable m00: float32; val mutable m01: float32; val mutable m02: float32; val mutable m03: float32
    val mutable m10: float32; val mutable m11: float32; val mutable m12: float32; val mutable m13: float32
    val mutable m20: float32; val mutable m21: float32; val mutable m22: float32; val mutable m23: float32
    val mutable m30: float32; val mutable m31: float32; val mutable m32: float32; val mutable m33: float32
end with
    new( a: float32 [] ) = {
        m00 = a.[0];  m01 = a.[1];  m02 = a.[2];  m03 = a.[3]
        m10 = a.[4];  m11 = a.[5];  m12 = a.[6];  m13 = a.[7]
        m20 = a.[8];  m21 = a.[9];  m22 = a.[10]; m23 = a.[11]
        m30 = a.[12]; m31 = a.[13]; m32 = a.[14]; m33 = a.[15]
    }
    new( x: Vec4, y: Vec4, z: Vec4, w: Vec4 ) = {
        m00 = x.x; m01 = x.y; m02 = x.z; m03 = x.w
        m10 = y.x; m11 = y.y; m12 = y.z; m13 = y.w
        m20 = z.x; m21 = z.y; m22 = z.z; m23 = z.w
        m30 = w.x; m31 = w.y; m32 = w.z; m33 = w.w
    }

    member inline v.Item = function | 0 -> v.x | 1 -> v.y | 2 -> v.z | 3 -> v.w | _ -> failwith "Index out of range"
    member inline v.x 
        with get() = Vec4( v.m00, v.m01, v.m02, v.m03 )
        and set (a: Vec4) = v.m00 <- a.x; v.m01 <- a.y; v.m02 <- a.z; v.m03 <- a.w

    member inline v.y 
        with get() = Vec4( v.m10, v.m11, v.m12, v.m13 )
        and set (a: Vec4) = v.m10 <- a.x; v.m11 <- a.y; v.m12 <- a.z; v.m13 <- a.w

    member inline v.z 
        with get() = Vec4( v.m20, v.m21, v.m22, v.m23 )
        and set (a: Vec4) = v.m20 <- a.x; v.m21 <- a.y; v.m22 <- a.z; v.m23 <- a.w

    member inline v.w 
        with get() = Vec4( v.m30, v.m31, v.m32, v.m33 )
        and set (a: Vec4) = v.m30 <- a.x; v.m31 <- a.y; v.m32 <- a.w; v.m33 <- a.z

let kRadToDegScalar = 180.0f / float32 Math.PI
let kDegToRadScalar = 1.0f / kRadToDegScalar

let inline toRadians f = f * kDegToRadScalar
let inline toDegrees f = f * kRadToDegScalar

let clamp t mi mx = max mi (min t mx)
let smoothstep x edge0 edge1 =
    let t = clamp ((x - edge0) / (edge1 - edge0)) 0.0f 1.0f
    t * t * (3.0f - 2.0f * t)

let reflect (i: Vec3) (n: Vec3) =
    i - 2.0f * (n *. i) * n

let axisVec axis =
    let v = vec3 0.0f 0.0f 0.0f
    v.[axis] <- 1.0f
    v

// Cols Rows
let matCreateScale (v: Vector<float32>) =
    let n = v.Length
    matrix [
        for i in 0 .. n - 1 do
            yield [ for j in 0 .. n - 1 -> if j = i then v.[j] else 0.0f ] |> List.toSeq
    ]

let matNxMidentity m n =
    matrix [
        for i in 0 .. m - 1 do yield [ for j in 0 .. n - 1 -> if j = i then 1.0f else 0.0f ] |> List.toSeq
    ]
    
let mat3x3identity = matNxMidentity 3 3
let mat3x4identity = matNxMidentity 3 4
let mat4x4identity = matNxMidentity 4 4

let mat3x3to3x4 (small: Matrix<float32>) =
    let nr = vec3 0.0f 0.0f 0.0f
    Matrix.appendRow nr small

let mat3x3to4x4 (small: Matrix<float32>) newRow newCol =
    let nr = vec3 0.0f 0.0f 0.0f
    let nc = vec4 0.0f 0.0f 0.0f 1.0f
    Matrix.appendRow nr small |> Matrix.appendCol nc

let mat3x3CreateRotationAboutAxis (v: Vector<float32>) angleDeg =
    let angle = toRadians angleDeg
    let sinAng, cosAng = sin angle, cos angle
    let ax, ay, az = v.[0], v.[1], v.[2]
    let axSq, aySq, azSq = ax * ax, ay * ay, az * az

    matrix [
        [axSq + (1.0f - axSq) * cosAng;           ay * ax * (1.0f - cosAng) - az * sinAng; az * ax * (1.0f - cosAng) + ay * sinAng]
        [ax * ay * (1.0f - cosAng) + az * sinAng; aySq - (1.0f - aySq) * cosAng;           az * ay * (1.0f - cosAng) - ax * sinAng]
        [ax * az * (1.0f - cosAng) - ay * sinAng; ay * az * (1.0f - cosAng) + ax * sinAng; azSq + (1.0f - azSq) * cosAng          ]
    ]

let mat3x3CreateScale (v: Vector<float32>) =
    let mtx = mat3x3identity.Copy()
    mtx.[0,0] <- v.[0]
    mtx.[1,1] <- v.[1]
    mtx.[2,2] <- v.[2]

let mat3x4CreateTranslation (v: Vector<float32>) =
    let mtx = mat3x3identity.Copy()
    Matrix.appendRow v mtx

let mat3x4CreateLookat (origin: Vector<float32>) (target: Vector<float32>) angleDeg =
    let direction = (target - origin).GetUnitVector()
    let angle = toRadians angleDeg
    let rotation3x3 = mat3x3CreateRotationAboutAxis direction angle 
    Matrix.appendRow origin rotation3x3

let eulerYPRTo3x3Rotation (ypr: Vector<float32>) =
    let y, p, r = ypr.[0], ypr.[1], ypr.[2]
    let yawMat = mat3x3CreateRotationAboutAxis (vec3 0.0f 0.0f 1.0f) y
    let pitchMat = mat3x3CreateRotationAboutAxis (vec3 1.0f 0.0f 0.0f) p
    let rollMat = mat3x3CreateRotationAboutAxis (vec3 0.0f 1.0f 0.0f) r
    yawMat * pitchMat * rollMat
