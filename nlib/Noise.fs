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

// Permutation and gradient tables used by simplex and perlin
module Tables =
    let grad3Src = [|
        (1,1,0); (-1,1,0); (1,-1,0); (-1,-1,0);
        (1,0,1); (-1,0,1); (1,0,-1); (-1,0,-1);
        (0,1,1); (0,-1,1); (0,1,-1); (0,-1,-1); |]

    let grad4Src = [|
        (0,1,1,1); (0,1,1,-1); (0,1,-1,1); (0,1,-1,-1);
        (0,-1,1,1); (0,-1,1,-1); (0,-1,-1,1); (0,-1,-1,-1);
        (1,0,1,1); (1,0,1,-1); (1,0,-1,1); (1,0,-1,-1);
        (-1,0,1,1); (-1,0,1,-1); (-1,0,-1,1); (-1,0,-1,-1);
        (1,1,0,1); (1,1,0,-1); (1,-1,0,1); (1,-1,0,-1);
        (-1,1,0,1); (-1,1,0,-1); (-1,-1,0,1); (-1,-1,0,-1);
        (1,1,1,0); (1,1,-1,0); (1,-1,1,0); (1,-1,-1,0);
        (-1,1,1,0); (-1,1,-1,0); (-1,-1,1,0); (-1,-1,-1,0) |]

    let pSrc = [|
        151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180 |];

    type Grad3 =
        struct
            val X: float32
            val Y: float32
            val Z: float32
        end
        new(x, y, z) = { X= float32 x; Y = float32 y; Z = float32 z}

    type Grad4 =
        struct
            val X: float32
            val Y: float32
            val Z: float32
            val W: float32
        end
        new(x, y, z, w) = { X= float32 x; Y = float32 y; Z = float32 z; W = float32 w }

    let toGrad3 (ituple: int*int*int) = match ituple with | (a,b,c) -> new Grad3(a, b, c)
    let toGrad4 (ituple: int*int*int*int) = match ituple with | (a,b,c,d) -> new Grad4(a, b, c, d)

    let P = [| for i in 0 .. 255 do yield pSrc.[i]; for i in 0 .. 255 do yield pSrc.[i] |]
    let pMod16 = [| for pv in P do yield pv % 16 |]
    let grad3Table = [| for pv in P do yield toGrad3 grad3Src.[pv % 12] |]
    let grad4Table = [| for pv in P do yield toGrad4 grad4Src.[pv % 32] |]

    let inline dotGradient2 ix iy fx fy =
        let x = fx - float32 ix
        let y = fy - float32 iy
        let xb = ix &&& 0xFF
        let yb = iy &&& 0xFF
        let g = grad3Table.[P.[xb + P.[yb]]]
        g.X * x + g.Y * y

    let inline dotGradient3 ix iy iz fx fy fz =
        let x = fx - float32 ix
        let y = fy - float32 iy
        let z = fz - float32 iz
        let xb = ix &&& 0xFF
        let yb = iy &&& 0xFF
        let zb = iz &&& 0xFF
        let g = grad3Table.[xb + P.[yb + P.[zb]]]
        g.X * x + g.Y * y + g.Z * z

    let inline dotGradient2Index idx fx fy =
        let g = grad3Table.[int idx]
        g.X * fx + g.Y * fy

    let inline dotGradient3Index idx fx fy fz =
        let g = grad3Table.[int idx]
        g.X * fx + g.Y * fy + g.Z * fz

open Tables

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
let inline dquintic x = 30.0f  * x * x * (x * (x - 2.0f) + 1.0f)

let lerp t a b = (1.0f - t) * a + t * b

let ffloor q =
    if q < 0.0f then int (q - 1.0f) else int q

let ffloorf q =
    (if q < 0.0f then int (q - 1.0f) else int q) |> float32

let l0 = 73856093u
let l1 = 19349663u
let l2 = 83492791u

let inline spatialHashp x y =
    uint32 P.[x + P.[y]]

let inline spatialHash2 x y =
    let x = uint32 x + 1301u
    let y = uint32 y + 1571u
    let muls = x * l0 + y * l1
    muls ^^^ (muls >>> 9)

let inline spatialHash3 x y z =
    (uint32 x * l0) ^^^ (uint32 y * l1) ^^^ (uint32 z * l2)

let inline countArray arr =
    let (mins, maxs) = (Array.min arr, Array.max arr)
    let size = maxs - mins
    let counts = Array.zeroCreate<int> (size + 1)
    for p in arr do counts.[int p - mins] <- counts.[int p - mins] + 1
    counts

// Perlin simple noise
type Perlin2D() =
    let kNumRandomGradients = 256
    let kXNoiseGen = 1619
    let kYNoiseGen = 31337
    let kNoiseGenShift = 8
    let rng = XorShift() :> IRandom
    let mutable lattice = [| for i in 0 .. kNumRandomGradients-1 -> random2DUnitVector rng |]

#if OLD_G
    let dotGradient2 ix iy fx fy =
        let gridHash = ix * kXNoiseGen + iy * kYNoiseGen
        let gridHash = gridHash ^^^ (gridHash >>> kNoiseGenShift)
        let gridHash = gridHash % kNumRandomGradients
        tuple2Dot lattice.[gridHash] (fx - (float32 ix)) (fy - (float32 iy))
#endif
    let getValue (p: float32 []) =
        let (x, y) = (p.[0], p.[1])
        let ix0 = ffloor x
        let ix1 = ix0 + 1
        let iy0 = ffloor y
        let iy1 = iy0 + 1
        let fx = x - (float32 ix0)
        let fy = y - (float32 iy0)

        // 6t^5 - 10t^4 + 15t^2
        let u = quintic fx
        let v = quintic fy
        let du = dquintic fx
        let dv = dquintic fy

        let a = dotGradient2 ix0 iy0 x y
        let b = dotGradient2 ix1 iy0 x y
        let c = dotGradient2 ix0 iy1 x y
        let d = dotGradient2 ix1 iy1 x y

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

    #if OLD_G
    let dotGradient ix iy iz fx fy fz =
        let gridHash = ix * kXNoiseGen + iy * kYNoiseGen + iz * kZNoiseGen
        let gridHash = gridHash ^^^ (gridHash >>> kNoiseGenShift)
        let gridHash = gridHash % kNumRandomGradients
        tuple3Dot lattice.[gridHash] (fx - (float32 ix)) (fy - (float32 iy)) (fz - (float32 iz)) * 1.63f
    #endif

    let getValue (p: float32 []) =
        let (x, y, z) = (p.[0], p.[1], p.[2])
        let ix0 = ffloor x
        let ix1 = ix0 + 1
        let iy0 = ffloor y
        let iy1 = iy0 + 1
        let iz0 = ffloor z
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

        let a = dotGradient3 ix0 iy0 iz0 x y z
        let b = dotGradient3 ix1 iy0 iz0 x y z
        let c = dotGradient3 ix0 iy1 iz0 x y z
        let d = dotGradient3 ix1 iy1 iz0 x y z
        let e = dotGradient3 ix0 iy0 iz1 x y z
        let f = dotGradient3 ix1 iy0 iz1 x y z
        let g = dotGradient3 ix0 iy1 iz1 x y z
        let h = dotGradient3 ix1 iy1 iz1 x y z

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

type Simplex2D() =
    let skew = 0.5f * (sqrt 3.0f - 1.0f)
    let unskew = (3.0f - sqrt 3.0f) / 6.0f;

    let getValue (p: float32 []) =
        let (x, y) = (p.[0],p.[1])

        let s = (x + y) * skew

        let i = (x + s) |> ffloorf
        let j = (y + s) |> ffloorf

        let t = (i + j) * unskew
        let (X0, Y0) = (i - t, j - t)
        let (x0, y0) = (x - X0, y - Y0)
        let (i1, j1) = if x0 > y0 then (1, 0) else (0, 1)
        let (x1, y1) = (x0 - (float32 i1) + unskew, y0 - (float32 j1) + unskew)
        let (x2, y2) = (x0 - 1.0f + 2.0f * unskew, y0 - 1.0f + 2.0f * unskew)
        let (ii, jj) = (int i &&& 0xFF, int j &&& 0xFF)
#if X
        let gi0 = pMod12.[ii + P.[jj]]
        let gi1 = pMod12.[ii + i1 + P.[jj + j1]]
        let gi2 = pMod12.[ii + 1 + P.[jj + 1]]
#else
#endif
        let t0 = 0.5f - x0 * x0 - y0 * y0
        let t1 = 0.5f - x1 * x1 - y1 * y1
        let t2 = 0.5f - x2 * x2 - y2 * y2

        let n0 =
            if t0 < 0.0f then 0.0f else
                let t0s = t0 * t0
                let gi0 = (spatialHash2 ii jj) % 12u
                t0s * t0s * dotGradient2Index gi0 x0 y0
        let n1 =
            if t1 < 0.0f then 0.0f else
                let t1s = t1 * t1
                let gi1 = (spatialHash2 (ii + i1) (jj + j1)) % 12u
                t1s * t1s * dotGradient2Index gi1 x1 y1
        let n2 =
            if t2 < 0.0f then 0.0f else
                let t2s = t2 * t2
                let gi2 = (spatialHash2 (ii + 1) (jj + 1)) % 12u
                t2s * t2s * dotGradient2Index gi2 x2 y2

        [| 70.0f * (n0 + n1 + n2) |]

    interface INoise with
        member __.Seed( seed ) = ()
        member __.GetValue( v ) = getValue v

type Simplex3D() =
    let skew = 1.0f / 3.0f
    let unskew = 1.0f / 6.0f

    let getValue (p: float32 []) =
        let (x, y, z) = (p.[0], p.[1], p.[2])

        let s = (x + y + z) * skew

        let i = (x + s) |> ffloorf
        let j = (y + s) |> ffloorf
        let k = (z + s) |> ffloorf

        let t = (i + j + k) * unskew
        let x0 = x - (i - t)
        let y0 = y - (j - t)
        let z0 = z - (k - t)

        let i1, j1, k1, i2, j2, k2 =
            if x0 >= y0 then
                if y0 >= z0 then
                    (1.0f, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f)
                else if x0 >= z0 then
                    (1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                else
                    (0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f)
            else
                if y0 < z0 then
                    (0.0f, 0.0f, 1.0f, 0.0f, 1.0f, 1.0f)
                else if x0 < z0 then
                    (0.0f, 1.0f, 0.0f, 0.0f, 1.0f, 1.0f)
                else
                    (0.0f, 1.0f, 0.0f, 1.0f, 1.0f, 0.0f)

        let x1 = x0 - i1 + unskew
        let y1 = y0 - j1 + unskew
        let z1 = z0 - k1 + unskew

        // let (x2, y2, z2) = (x0 - if2 + 2.0f * unskew, y0 - jf2 + 2.0f * unskew, z0 - kf2 + 2.0f * unskew)
        let x2 = x0 - i2 + 2.0f * unskew
        let y2 = y0 - j2 + 2.0f * unskew
        let z2 = z0 - k2 + 2.0f * unskew

        // let (x3, y3, z3) = (x0 - 1.0f + 3.0f * unskew, y0 - 1.0f + 3.0f * unskew, z0 - 1.0f + 3.0f * unskew)
        let x3 = x0 - 1.0f + 3.0f * unskew
        let y3 = y0 - 1.0f + 3.0f * unskew
        let z3 = z0 - 1.0f + 3.0f * unskew

        #if X
        let gi0 = P.[ii + P.[jj + P.[kk]]] &&& 0xF
        let gi1 = P.[ii + i1 + P.[jj + j1 + P.[kk + k1]]] &&& 0xF
        let gi2 = P.[ii + i2 + P.[jj + j2 + P.[kk + k2]]] &&& 0xF
        let gi3 = P.[ii + 1 + P.[jj + 1 + P.[kk + 1]]] &&& 0xF
        #endif

        let t0 = 0.6f - x0 * x0 - y0 * y0 - z0 * z0
        let t1 = 0.6f - x1 * x1 - y1 * y1 - z1 * z1
        let t2 = 0.6f - x2 * x2 - y2 * y2 - z2 * z2
        let t3 = 0.6f - x3 * x3 - y3 * y3 - z3 * z3

        let inline hashIdx i j k = (spatialHash3 (int i) (int j) (int k)) % 12u

        let n0 =
            if t0 < 0.0f then 0.0f else
                let t0s = t0 * t0
                t0s * t0s * dotGradient3Index (hashIdx i j k) x0 y0 z0
        let n1 =
            if t1 < 0.0f then 0.0f else
                let t1s = t1 * t1
                t1s * t1s * dotGradient3Index (hashIdx (i + i1) (j + j1) (k + k1)) x1 y1 z1
        let n2 =
            if t2 < 0.0f then 0.0f else
                let t2s = t2 * t2
                t2s * t2s * dotGradient3Index (hashIdx (i + i2) (j + j2) (k + k2)) x2 y2 z2
        let n3 =
            if t3 < 0.0f then 0.0f else
                let t3s = t3 * t3
                t3s * t3s * dotGradient3Index (hashIdx (i + 1.0f) (j + 1.0f) (k + 1.0f)) x3 y3 z3

        [| 32.0f * (n0 + n1 + n2 + n3) |]

    interface INoise with
        member __.Seed( seed ) = ()
        member __.GetValue( v ) = getValue v

type BitNoise3D() =
    let bitTable = [| 0x15; 0x38; 0x32; 0x2c; 0x0d; 0x13; 0x07; 0x2a |]

    let getValue( p: float32[] ) =
        let x, y, z = p.[0], p.[1], p.[2]

        let s = (x + y + z) / 3.0f
        let i, j, k = x + s |> ffloorf, y + s |> ffloorf, z + s |> ffloorf
        let s = (i + j + k) / 6.0f
        let u, v, w = x - i + s, y - j + s, z - k + s

        let mutable A = [| 0.0f; 0.0f; 0.0f |]

        let inline b0 N B = N >>> B &&& 1
        let inline b i j k B = bitTable.[((b0 i B) <<< 2) ||| ((b0 j B) <<< 1) ||| (b0 k B)]
        let inline shuffle i j k =
            b i j k 0 + b j k i 1 + b k i j 2 + b i j k 3 +
            b j k i 4 + b k i j 5 + b i j k 6 + b j k i 7

        let inline K a =
            let A0, A1, A2 = (A.[0], A.[1], A.[2])
            let s = (A0 + A1 + A2) / 6.0f
            let x, y, z = (u - A0 + s, v - A1 + s, w - A2 + s)
            let t = 0.5f - x*x - y*y - z*z
            let h = shuffle (int (i + A0)) (int (j + A1)) (int (k + A2))
            A.[a] <- A.[a] + 1.0f
            if t < 0.0f then 0.0f else
                let b5, b4, b3, b2, b = (b0 h 5, b0 h 4, b0 h 3, b0 h 2, h &&& 3)
                let p = if b = 1 then x else if b = 2 then y else z
                let q = if b = 1 then y else if b = 2 then z else x
                let r = if b = 1 then z else if b = 2 then x else y
                let p = if b5 = b3 then -p else p
                let q = if b5 = b4 then -q else q
                let r = if b5 <> b4 ^^^ b3 then -r else r
                let t' = t * t
                8.0f * t * t * (p + if b = 0 then q + r else if b2 = 0 then q else r)
        
        let hi =
            if u >= w then
                if u >= v then 0 else 1
            else if v >= w then 1 else 2
        let lo =
            if u < w then
                if u < v then 0 else 1
            else if v < w then 1 else 2
        [| K hi + K (3 - hi - lo) + K lo + K 0 |]

    interface INoise with
        member __.Seed( seed ) = ()
        member __.GetValue( v ) = getValue v

type FBM() =
    let noise = Perlin2D() :> INoise

    let getValue( p: float32[] ) =
        let octaves = int p.[2]
        let mutable scale = 1.0f
        let mutable fade  = 0.5f
        let mutable lacu  = 2.0f
        let mutable dx, dz = 0.0f, 0.0f

        let mutable r = 0.0f
        for i in 0 .. octaves - 1 do
            let va= noise.GetValue( p )
            dx <- dx + va.[1]
            dz <- dz + va.[2]
            let nv = scale * va.[0] / (1.0f + dx*dx + dz*dz)
            r <- r + nv
            scale <- scale * fade
            p.[0] <- p.[0] * lacu
            p.[1] <- p.[1] * lacu
            p.[2] <- p.[2] * lacu

        [| (r - 0.2f) * 1.5f |]

    interface INoise with
        member __.Seed( seed ) = ()
        member __.GetValue( v ) = getValue v

open System.Windows
open System.Windows.Media.Imaging

let testPerlin() =
    let n = 256
    let mutable xs = 0.01f
    let mutable ys = 0.01f
    let mutable zs = 0.01f
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
    // let noiseSrc = new Perlin3D() :> INoise
    // let noiseSrc = new Simplex2D() :> INoise
    // let noiseSrc = new Simplex3D() :> INoise
    // let noiseSrc = new BitNoise3D() :> INoise
    let noiseSrc = new FBM() :> INoise
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
               let nv = noiseSrc.GetValue( [| (float32 x) * xs+ x0; (float32 y) * ys + y0; z0; 8.0f |] ).[0] * 0.5f + 0.5f
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
    let mutable xs = 0.02f
    let mutable ys = 0.02f
    let mutable zs = 0.02f
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
