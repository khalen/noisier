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
    (sin angle, cos angle)

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
        (tuple2Dot lattice.[gridHash] fx fy) 

    let getValue (p: float32 []) =
        let (x, y) = (p.[0], p.[1])
        let ix0 = int (floor x)
        let ix1 = ix0 + 1
        let iy0 = (floor y) |> int
        let iy1 = iy0 + 1
        let fx = x - (float32 ix0)
        let fy = y - (float32 iy0)

        let s = sprintf "%d %d %f %f %f %f\n" ix0 iy0 fx fy x y
        System.Diagnostics.Debug.WriteLine( s )

        // 6t^5 - 10t^4 + 15t^2
        // let u = quintic fx
        // let v = quintic fy
        let u = fx
        let v = fy

        let du = dquintic fx
        let dv = dquintic fy

        let a = dotGradient ix1 iy0 fx fy
        let b = dotGradient ix0 iy0 fx fy
        let c = dotGradient ix1 iy1 fx fy
        let d = dotGradient ix0 iy1 fx fy

        let k0 = a              // -1 .. 1
        let k1 = b - a          // -1 .. 1 - (-1 .. 1) = -2 .. 2
        let k2 = c - a          // -2 .. 2
        let k3 = a - b - c + d  // 

        // [| k0 + k1 * u + k2 * v + k3 * u * v; du * (k1 + k3 * v); dv * (k2 + k3 * u) |]
        // [| lerp v (lerp u a b) (lerp u c d); du * (k1 + k3 * v); dv * (k2 + k3 * u) |]
        [| (float32 ix0) / 256.0f * 2.0f - 1.0f; du * (k1 + k3 * v); dv * (k2 + k3 * u) |]

    let reseed rng = 
        lattice <- [| for i in 0 .. kNumRandomGradients-1 -> random2DUnitVector rng |]

    interface INoise with
        member __.Seed( seed ) = reseed ((new XorShift( seed )) :> IRandom)
        member __.GetValue( v ) = getValue v

open System.Windows
open System.Windows.Media.Imaging

let testPerlin() =
    let n = 256
    let mutable xs = 0.5f
    let mutable ys = 0.5f
    let mutable x0 = 0.23f
    let mutable y0 = 0.23f
    let image = Controls.Image( Stretch = Media.Stretch.Uniform )    
    let minLabel = Controls.Label()
    let maxLabel = Controls.Label()
    let format = Media.PixelFormats.Gray8
    let pixel = Array.create (n * n) 0uy
    let noises = Array.create (n * n) 0.0f
    let noiseSrc = new Perlin2D() :> INoise
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
    let update _ =
        for y in 0 .. n-1 do
            for x in 0 .. n-1 do
               let nv = noiseSrc.GetValue( [| (float32 x) * xs+ x0; (float32 y) * ys + y0 |] ).[0] * 0.5f + 0.5f
               pixel.[x + y * n] <- clamp (nv * 256.0f) 0.0f 255.0f |> uint8
               noises.[x + y * n] <- nv

        x0 <- x0 + 0.011f
        y0 <- y0 + 0.011f

        image.Source <-
            BitmapSource.Create( n, n, 1.0, 1.0, format, null, pixel, n )
        let minn = noises |> Array.min
        let maxn = noises |> Array.max
        minLabel.Content <- (sprintf "Min: %f" minn)
        maxLabel.Content <- (sprintf "Max: %f" maxn)

    Media.CompositionTarget.Rendering.Add update
    Window( Content = panel, Title = "Noise Test" )
        |> (Application()).Run |> ignore
