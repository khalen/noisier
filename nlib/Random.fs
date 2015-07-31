module Random

open System

let kRandFloat32Mul = 1.0f / (float32 UInt32.MaxValue)
let kRandFloatMul = 1.0 / (float UInt64.MaxValue)

type IRandom = 
    abstract member GetFloat32 : unit -> float32
    abstract member GetFloat : unit -> float
    abstract member GetUint64 : unit -> uint64
    abstract member GetUint64Range : uint64 -> uint64 -> uint64
    abstract member Seed : uint64 -> unit

type XorShift() =
    let mutable state = Array.zeroCreate<uint64> 2

    let reseed (rng: Random) =
        let hiloUint64 a b = ((uint64 a) <<< 32) ||| (uint64 b)
        state.[0] <- hiloUint64 (rng.Next()) (rng.Next())
        state.[1] <- hiloUint64 (rng.Next()) (rng.Next())

    do
        reseed (new Random())

    new( seed ) as self =
        new XorShift() then (self :> IRandom).Seed( seed )

    interface IRandom with
        member v.GetUint64() =
            let x = state.[0]
            let y = state.[1]

            let x = x ^^^ (x <<< 23)
            let x = x ^^^ (x >>> 17)
            let x = x ^^^ (y ^^^ (y >>> 26))
             
            state.[0] <- y
            state.[1] <- x
            x + y 

        member v.GetFloat32() =
            let rval = (v :> IRandom).GetUint64() |> uint32 |> float32
            rval * kRandFloat32Mul

        member v.GetFloat() =
            let rval = (v :> IRandom).GetUint64() |> float
            rval * kRandFloatMul

        member v.GetUint64Range mn mx =
            let delta = mx - mn + 1UL
            let rval = (v :> IRandom).GetUint64()
            rval % delta + mn

        member v.Seed sv =
            reseed (new Random((int32)sv &&& 0xFFFFFFFF))
