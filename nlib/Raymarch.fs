module Raymarch

open FsAlg.Generic
open BaseTypes
open Camera
open Math

type Color = Vec
type Ray = { origin: Vec; direction: Vec }

let lerpColor t (x: Color) (y: Color) =
    (1.0f - t) * x + t * y

let sunDir = (vec3 0.5f -0.2f 0.3f).GetUnitVector()
let sunColor = vec3 0.8f 0.6f 3.0f
let ambient = vec3 0.2f 0.2f 0.2f

let saturate t =
    max 0.0f (min t 1.0f)

let saturateVec (v: Vec) =
    vec3 (saturate v.[0]) (saturate v.[1]) (saturate v.[2])

let colorOfSurfaceAtPt (_: Vec) (normal: Vec) =
    (saturate (normal * sunDir)) * sunColor + ambient
    
let skyColor (skyHitPt: Vec) = lerpColor (skyHitPt.[1] / 1000.0f) (vec3 0.1f 0.3f 0.1f) (vec3 0.6f 0.8f 0.6f)

let traceRay (func: float32 -> float32 -> float32) ray =
    let origin = ray.origin
    let dir = ray.direction
    let kMaxDist = 10.0f
    let mutable stepDist = 0.01f
    let mutable t = 0.001f
    let mutable r = (0.0f, false)
    let mutable lastHeight = 0.0f
    let mutable lastY = 0.0f

    while t < kMaxDist do
        let sample = origin + dir * t
        let height  = func sample.[0] sample.[2]
        if sample.[1] < height then do // Hit!
            r <- (t, true)
            t <- kMaxDist
        else
            t <- t + stepDist
            stepDist <- 0.01f * t
            lastHeight <- height
            lastY <- sample.[1]

    r

let createRay (cam: Camera) x y sx sy =
    let x, y = float32 x, float32 y
    let sx, sy = float32 sx, float32 sy
    let fov = cam.Fov
    let screenX = (-sx + x * 2.0f) / sy
    let screenY = (-sy + y * 2.0f) / sy
    let ct = cam :> ITransform
    let direction = (ct.Orientation * (vec3 screenX screenY fov)).GetUnitVector()
    {
        origin = ct.Position
        direction = direction
    }

let getTerrainNormal (pt: Vec) func =
    let kEpsilon = 0.001f
    let px, _, pz = pt.[0], pt.[1], pt.[2]
    let normal = vec3 (func (px - kEpsilon) pz - func (px + kEpsilon) pz)
                      (2.0f * kEpsilon)
                      (func px (pz - kEpsilon) - func px (pz + kEpsilon))
    normal.GetUnitVector()

let getTerrainColor ray t func =
    let hitPt = ray.origin + ray.direction * t
    let hitNormal = getTerrainNormal hitPt func
    colorOfSurfaceAtPt hitPt hitNormal

let getSkyColor _ _ _ =
    vec3 0.3f 0.8f 0.6f

let getRayColor ray (t, hit) func =
    if hit then getTerrainColor ray t func
    else getSkyColor ray t func

// Assume 0.0 = center of screen; "near plane" is at 1z and 1 unit in each direction so -0.5f -> 0.5f in x and y
let rayMarch (cam: Camera) (func: float32 -> float32 -> float32) sx sy (pixels: uint16 []) =
    for yi in 0 .. sy - 1 do
        for xi in 0 .. sx - 1 do
            let ray = createRay cam xi yi sx sy
            let hit = traceRay func ray
            let color = getRayColor ray hit func
            let xelIdx = (xi + (sy - 1 - yi) * sx) * 3
            pixels.[xelIdx + 0] <- uint16 (color.[0] * 65536.0f)
            pixels.[xelIdx + 1] <- uint16 (color.[1] * 65536.0f)
            pixels.[xelIdx + 2] <- uint16 (color.[2] * 65536.0f)
