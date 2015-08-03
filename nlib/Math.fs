module Math

open FsAlg.Generic
open System

// Random math funcs

let kRadToDegScalar = 180.0f / float32 Math.PI
let kDegToRadScalar = 1.0f / kRadToDegScalar

let inline toRadians f = f * kDegToRadScalar
let inline toDegrees f = f * kRadToDegScalar

let vec3 x y z = vector [| x; y; z |]
let vec4 x y z w = vector [| x; y; z; w |]

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
