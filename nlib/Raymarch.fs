module Raymarch

open FsAlg.Generic
open BaseTypes
open Camera
open Math
open Noise

type Color = Vec
type Ray = { origin: Vec; direction: Vec }

let lerpColor t (x: Color) (y: Color) =
    (1.0f - t) * x + t * y

let gamma (v: Vec) g =
    vec3 (v.[0] ** g) (v.[1] ** g) (v.[2] ** g)

let sunDir = (vec3 -0.5f -0.2f -0.3f).GetUnitVector()
let sunColor = vec3 1.0f 0.8f 0.8f
let ambient = vec3 0.2f 0.2f 0.2f

let inline saturate t =
    max 0.0f (min t 1.0f)

let inline saturateVec (v: Vec) =
    vec3 (saturate v.[0]) (saturate v.[1]) (saturate v.[2])

let inline colorOfSurfaceAtPt (_: Vec) (_: Vec) =
    vec3 0.1f 0.1f 0.1f
    
let inline skyColor (skyHitPt: Vec) = lerpColor (skyHitPt.[1] / 1000.0f) (vec3 0.1f 0.3f 0.1f) (vec3 0.6f 0.8f 0.6f)

#if STEP
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
            let rest = t - stepDist + stepDist * (lastHeight - lastY) / (sample.[1] - lastY - height + lastHeight)
            r <- (rest, true)
            t <- kMaxDist + 1.0f
        else
            t <- t + stepDist
            stepDist <- 0.01f * t
            lastHeight <- height
            lastY <- sample.[1]

    r
#else
let traceRay (func: float32 -> float32 -> float32) ray tmin tmax =
    let origin = ray.origin
    let dir = ray.direction
    let kMaxIters = 256
    let mutable r = (0.0f, false)
    let mutable t = tmin
    let mutable iters = 0
    while iters < kMaxIters && t < tmax do
        let samplePt = origin + dir * t
        let dist = samplePt.[1] - func samplePt.[0] samplePt.[2]
        if dist < 0.001f then do
            r <- (t, true)        
            iters <- kMaxIters
        iters <- iters + 1
        t <- t + dist * 0.5f
    if iters >= kMaxIters then do r <- (t, true)
    r
#endif

let SC = 250.0f
let shadowRay (func: float32 -> float32 -> float32) ray =
    let ro = ray.origin
    let rd = ray.direction
    let mutable res = 1.0f
    let mutable t = 0.001f
    let mutable reps = 0
    while reps < 128 do
        let p = ro + t * rd
        let h = p.[1] - func p.[0] p.[2]
        res <- min res (16.0f * h / t)
        t <- t + h
        if res < 0.001f || p.[1] > (SC * 200.0f) then do
            reps <- 128
        reps <- reps + 1
    clamp res 0.0f 1.0f

let createRay (cam: Camera) x y sx sy =
    let x, y = float32 x, float32 y
    let sx, sy = float32 sx, float32 sy
    let fov = cam.Fov
    let screenX = (-sx + x * 2.0f) / sy
    let screenY = (-sy + y * 2.0f) / sy
    let camAxis = cam.Axis
    let direction = (screenX * camAxis.right + screenY * camAxis.up + fov * camAxis.fwd).GetUnitVector()
    {
        origin = cam.Origin
        direction = direction
    }

let light1 = (vec3 -0.8f 0.4f -0.1f).GetUnitVector()

let noiseGen = Perlin2D() :> INoise

let getTerrainNormal (pt: Vec) t func =
    let kEpsilon = 0.002f * t
    let px, _, pz = pt.[0], pt.[1], pt.[2]
    let normal = vec3 (func (px - kEpsilon) pz - func (px + kEpsilon) pz)
                      (2.0f * kEpsilon)
                      (func px (pz - kEpsilon) - func px (pz + kEpsilon))
    normal.GetUnitVector()

let getTerrainColor hitPt norm t =
    colorOfSurfaceAtPt hitPt norm

let getSkyColor _ _ _ =
    vec3 0.3f 0.8f 0.6f

let powVec (v:Vec) power =
    vec3 (v.[0] ** power) (v.[1] ** power) (v.[2] ** power)

let getRayColor ray (t, hit) func =
    let ro = ray.origin
    let rd = ray.direction
    let sundot = clamp (light1 * rd) 0.0f 1.0f

    if t < 0.0f then
        vec3 1.0f 1.0f 0.0f

    else if not hit then
        let col = (vec3 0.3f 0.55f 0.8f) * (1.0f - 0.8f * rd.[1]) * 0.9f

        // sun
        let col = col + 0.25f * (vec3 1.0f 0.7f 0.4f) * sundot ** 5.0f
        let col = col + 0.25f * (vec3 1.0f 0.8f 0.6f) * sundot ** 64.0f
        let col = col + 0.2f  * (vec3 1.0f 0.8f 0.6f) * sundot ** 512.0f

        // horizon
        lerpColor ((1.0f - max rd.[1] 0.0f) ** 8.0f) col (vec3 0.7f 0.75f 0.8f)

    else 
        let hitPt = ray.origin + ray.direction * t
        let norm = getTerrainNormal hitPt t func

        let col = getTerrainColor hitPt norm t
        let amb = clamp (0.5f + 0.5f * norm.[1]) 0.0f 1.0f
        let dif = clamp (light1 * norm) 0.0f 1.0f
        let sh =
            if dif >= 0.0001f then
                shadowRay func { origin = hitPt + light1 * 0.2f; direction = light1 }
            else
                1.0f

        let flatlight1 = (vec3 -light1.[0] 0.0f light1.[2]).GetUnitVector()
        let bac = clamp (0.2f + 0.8f * flatlight1 * norm) 0.0f 1.0f

        let lin = dif * (vec3 7.0f 5.0f 3.0f) .* (vec3 sh (sh * sh * 0.5f + 0.5f * sh) (sh * sh * 0.8f + 0.2f * sh))
        let lin = lin + amb * (vec3 0.40f 0.60f 0.80f) * 1.2f
        let lin = lin + bac * (vec3 0.40f 0.50f 0.60f)
        let col = col .* lin
         
        let fo = 1.0f - exp (-0.001f * t / SC)
        let fco = 0.7f * (vec3 0.5f 0.7f 0.9f) + 0.1f * (vec3 1.0f 0.8f 0.5f) * (sundot ** 4.0f)
        let col = lerpColor fo col fco
        //let col = col + 0.3f * (vec3 1.0f 0.8f 0.4f) * (sundot ** 8.0f) * (1.0f - exp (-0.002f * t / SC))
        col

// Assume 0.0 = center of screen; "near plane" is at 1z and 1 unit in each direction so -0.5f -> 0.5f in x and y
let rayMarch (cam: Camera) (func: float32 -> float32 -> float32) sx sy (pixels: float32 []) =
    for yi in 0 .. sy - 1 do
        for xi in 0 .. sx - 1 do
            let ray = createRay cam xi yi sx sy
            let hit = traceRay func ray 0.001f 1000.0f
            let color = getRayColor ray hit func
            let xelIdx = (xi + (sy - 1 - yi) * sx) * 4
            pixels.[xelIdx + 0] <- color.[0]
            pixels.[xelIdx + 1] <- color.[1]
            pixels.[xelIdx + 2] <- color.[2]
            pixels.[xelIdx + 3] <- 1.0f
