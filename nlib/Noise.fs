module Noise

open System
open Random

//const int X_NOISE_GEN = 1619;
//const int Y_NOISE_GEN = 31337;
//const int Z_NOISE_GEN = 6971;
//const int SEED_NOISE_GEN = 1013;
//const int SHIFT_NOISE_GEN = 8;

type INoise =
    abstract member Seed : uint64 -> unit
    abstract member GetValue : float32 [] -> float32 []

let random2DUnitVector( rng: IRandom ) =
    // Random angle in radians
    let angle = (rng.GetFloat32()) * 2.0f * (Math.PI |> float32)
    let x = sin angle
    let y = cos angle
    let mag = sqrt (x * x + y * y)
    (x / mag, y / mag)

let random3DUnitVector( rng: IRandom ) =
    let theta = rng.GetFloat() * 2.0 * Math.PI
    let cost = cos theta
    let sint = sin theta
    let z = rng.GetFloat() * 2.0 - 1.0
    let omzsq = sqrt (1.0 - z*z)
    (omzsq * cost |> float32, omzsq * sint |> float32, z |> float32)

let inline tuple2Dot (ax, ay) bx by = ax * bx + ay * by
let inline tuple3Dot (ax, ay, az) bx by bz = ax * bx + ay * by + az * bz

// u(x) = -2x^3 + 3x^2
let inline cubic u = u * (u * (u * -2.0f) + 3.0f)

// u'(x) = -6x^2 + 6
let inline dcubic u = -6.0f * u * u + 6.0f

// u(x) = 6x^5 + -15x^4 + 10x^3
let inline quintic u = u * u * u * (u * (u * 6.0f - 15.0f) + 10.0f)

// u'(x) = 30x^4 + -60x^3 + 30x^2
let inline dquintic u = u * u * (u * (u * 30.0f - 60.0f) + 30.0f)

let lerp t a b = (1.0f - t) * a + t * b

// Perlin simple noise
type Perlin2D() =
    let kNumRandomGradients = 256
    let kXNoiseGen = 1619
    let kYNoiseGen = 31337
    let kNoiseGenShift = 8
    let rng = XorShift() :> IRandom
    let mutable lattice = [| for i in 0 .. kNumRandomGradients-1 -> random2DUnitVector rng |]

    let dotGradient ix iy fx fy =
        let gridHash = ix * kXNoiseGen + iy * kYNoiseGen
        let gridHash = gridHash ^^^ (gridHash >>> kNoiseGenShift)
        let gridHash = gridHash % kNumRandomGradients
        tuple2Dot lattice.[gridHash] (fx - (float32 ix)) (fy - (float32 iy))

    let getValue (p: float32 []) =
        let (x, y) = (p.[0], p.[1])
        let ix0 = int (floor x)
        let ix1 = ix0 + 1
        let iy0 = int (floor y)
        let iy1 = iy0 + 1
        let fx = x - (float32 ix0)
        let fy = y - (float32 iy0)

        // 6t^5 - 10t^4 + 15t^2
        let u = quintic fx
        let v = quintic fy
        let du = dquintic fx
        let dv = dquintic fy

        let a = dotGradient ix0 iy0 x y
        let b = dotGradient ix1 iy0 x y
        let c = dotGradient ix0 iy1 x y
        let d = dotGradient ix1 iy1 x y

        let k0 = a              // -1 .. 1
        let k1 = b - a          // -1 .. 1 - (-1 .. 1) = -2 .. 2
        let k2 = c - a          // -2 .. 2
        let k3 = a - b - c + d  // 

        [| k0 + k1 * u + k2 * v + k3 * u * v; du * (k1 + k3 * v); dv * (k2 + k3 * u) |]

    let reseed rng = 
        lattice <- [| for i in 0 .. kNumRandomGradients-1 -> random2DUnitVector rng |]

    interface INoise with
        member __.Seed( seed ) = reseed ((new XorShift( seed )) :> IRandom)
        member __.GetValue( v ) = getValue v

type Perlin3D() =
    let kNumRandomGradients = 256
    let kXNoiseGen = 1619
    let kYNoiseGen = 31337
    let kZNoiseGen = 6971
    let kNoiseGenShift = 8
    let rng = XorShift() :> IRandom
    let mutable lattice = [| for i in 0 .. kNumRandomGradients-1 -> random3DUnitVector rng |]

    let dotGradient ix iy iz fx fy fz =
        let gridHash = ix * kXNoiseGen + iy * kYNoiseGen + iz * kZNoiseGen
        let gridHash = gridHash ^^^ (gridHash >>> kNoiseGenShift)
        let gridHash = gridHash % kNumRandomGradients
        tuple3Dot lattice.[gridHash] (fx - (float32 ix)) (fy - (float32 iy)) (fz - (float32 iz)) * 1.63f

    let getValue (p: float32 []) =
        let (x, y, z) = (p.[0], p.[1], p.[2])
        let ix0 = int (floor x)
        let ix1 = ix0 + 1
        let iy0 = int (floor y)
        let iy1 = iy0 + 1
        let iz0 = int (floor z)
        let iz1 = iz0 + 1
        let fx = x - (float32 ix0)
        let fy = y - (float32 iy0)
        let fz = z - (float32 iz0)

        // 6t^5 - 10t^4 + 15t^2
        let u = quintic fx
        let v = quintic fy
        let w = quintic fz
        let du = dquintic fx
        let dv = dquintic fy
        let dw = dquintic fz

        let a = dotGradient ix0 iy0 iz0 x y z
        let b = dotGradient ix1 iy0 iz0 x y z
        let c = dotGradient ix0 iy1 iz0 x y z
        let d = dotGradient ix1 iy1 iz0 x y z
        let e = dotGradient ix0 iy0 iz1 x y z
        let f = dotGradient ix1 iy0 iz1 x y z
        let g = dotGradient ix0 iy1 iz1 x y z
        let h = dotGradient ix1 iy1 iz1 x y z

        let k0 = a              // -1 .. 1
        let k1 = b - a          // -1 .. 1 - (-1 .. 1) = -2 .. 2
        let k2 = c - a          // -2 .. 2
        let k3 = e - a
        let k4 = a - b - c + d
        let k5 = a - c - e + g
        let k6 = a - b - e + f
        let k7 = -a + b + c - d + e - f - g + h

        let v = k0 + k1*u + k2*v + k3*w + k4*u*v + k5*v*w + k6*w*u + k7*u*v*w
        let vdu = du * (k1 + k4*v + k6*w + k7*v*w)
        let vdv = dv * (k2 + k5*w + k4*u + k7*w*u)
        let vdw = dw * (k3 + k6*u + k5*v + k7*u*v)
        [| v; vdu; vdv; vdw |]

    let reseed rng = 
        lattice <- [| for i in 0 .. kNumRandomGradients-1 -> random3DUnitVector rng |]

    interface INoise with
        member __.Seed( seed ) = reseed ((new XorShift( seed )) :> IRandom)
        member __.GetValue( v ) = getValue v

open System.Windows
open System.Windows.Media.Imaging

let terra
let testPerlin() =
    let n = 256
    let mutable xs = 0.1f
    let mutable ys = 0.1f
    let mutable zs = 0.1f
    let mutable x0 = 0.23f
    let mutable y0 = 0.23f
    let mutable z0 = 0.23f
    let image = Controls.Image( Stretch = Media.Stretch.Uniform )    
    let minLabel = Controls.Label()
    let maxLabel = Controls.Label()
    let format = Media.PixelFormats.Gray8
    let pixel = Array.create (n * n) 0uy
    let noises = Array.create (n * n) 0.0f
    // let noiseSrc = new Perlin2D() :> INoise
    let noiseSrc = new Perlin3D() :> INoise
    let panel = Controls.DockPanel()
    panel.HorizontalAlignment <- HorizontalAlignment.Stretch
    panel.VerticalAlignment <- VerticalAlignment.Stretch
    let add (ctrl: UIElement) =
      Controls.DockPanel.SetDock(ctrl, Controls.Dock.Top)
      panel.Children.Add ctrl |> ignore
    add minLabel
    add maxLabel
    add image
    let clamp x min max = if x < min then min else if x > max then max else x
    let update p =
        for y in 0 .. n-1 do
            for x in 0 .. n-1 do
               let nv = noiseSrc.GetValue( [| (float32 x) * xs+ x0; (float32 y) * ys + y0; z0 |] ).[0] * 0.5f + 0.5f
               pixel.[x + y * n] <- clamp (nv * 256.0f) 0.0f 255.0f |> uint8
               noises.[x + y * n] <- nv

        x0 <- x0 + 0.02f
        y0 <- y0 + 0.02f
        z0 <- z0 + 0.021f

        image.Source <-
            BitmapSource.Create( n, n, 1.0, 1.0, format, null, pixel, n )
        let minn = noises |> Array.min
        let maxn = noises |> Array.max
        minLabel.Content <- (sprintf "Min: %f %A" minn p)
        maxLabel.Content <- (sprintf "Max: %f" maxn)

    Media.CompositionTarget.Rendering.Add update
    Window( Content = panel, Title = "Noise Test" )
        |> (Application()).Run |> ignore

let testPerlin1() =
    let n = 256
    let mutable xs = 0.1f
    let mutable ys = 0.1f
    let mutable zs = 0.1f
    let mutable x0 = 0.23f
    let mutable y0 = 0.23f
    let mutable z0 = 0.23f
    let image = Controls.Image( Stretch = Media.Stretch.Uniform )    
    let minLabel = Controls.Label()
    let maxLabel = Controls.Label()
    let format = Media.PixelFormats.Gray8
    let pixel = Array.create (n * n) 0uy
    let noises = Array.create (n * n) 0.0f
    // let noiseSrc = new Perlin2D() :> INoise
    let noiseSrc = new Perlin3D() :> INoise
    let panel = Controls.DockPanel()
    panel.HorizontalAlignment <- HorizontalAlignment.Stretch
    panel.VerticalAlignment <- VerticalAlignment.Stretch
    let add (ctrl: UIElement) =
      Controls.DockPanel.SetDock(ctrl, Controls.Dock.Top)
      panel.Children.Add ctrl |> ignore
    add minLabel
    add maxLabel
    add image
    let clamp x min max = if x < min then min else if x > max then max else x
    let update p =
        for y in 0 .. n-1 do
            for x in 0 .. n-1 do
               let nv = noiseSrc.GetValue( [| (float32 x) * xs+ x0; (float32 y) * ys + y0; z0 |] ).[0] * 0.5f + 0.5f
               pixel.[x + y * n] <- clamp (nv * 256.0f) 0.0f 255.0f |> uint8
               noises.[x + y * n] <- nv

        x0 <- x0 + 0.02f
        y0 <- y0 + 0.02f
        z0 <- z0 + 0.021f

        image.Source <-
            BitmapSource.Create( n, n, 1.0, 1.0, format, null, pixel, n )
        let minn = noises |> Array.min
        let maxn = noises |> Array.max
        minLabel.Content <- (sprintf "Min: %f %A" minn p)
        maxLabel.Content <- (sprintf "Max: %f" maxn)

    Media.CompositionTarget.Rendering.Add update
    Window( Content = panel, Title = "Noise Test" )
        |> (Application()).Run |> ignore
