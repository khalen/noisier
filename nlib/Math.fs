module Math

#nowarn "9"

open System
open System.Runtime.InteropServices

type Single with
    static member EqualsEpsilon = 0.00001f

type Vector2 =
    val mutable x: float32
    val mutable y: float32

    new( x, y ) = { x = x; y = y }
    new( x ) = { x = x; y = x }

    member inline a.Set( x', y' ) = a.x <- x'; a.y <- y'
    member inline a.Set( b: Vector2 ) = a.x <- b.x; a.y <- b.y
    member inline a.Set( v: float32[] ) = a.x <- v.[0]; a.y <- v.[1] 

    static member inline (+) (a: Vector2, b: Vector2) = Vector2( a.x + b.x, a.y + b.y )
    static member inline (+) (a: Vector2, b: float32) = Vector2( a.x + b, a.y + b )
    static member inline (+) (a: float32, b: Vector2) = Vector2( a + b.x, a + b.y )
    static member inline (+=) (a: Vector2, b: Vector2) = a.Set( a.x + b.x, a.y + b.y )
    static member inline (+=) (a: Vector2, b: float32) = a.Set( a.x + b, a.y + b )

    static member inline (~-) (a: Vector2) = Vector2( -a.x, -a.y )
    static member inline (-) (a: Vector2, b: Vector2) = Vector2( a.x - b.x, a.y - b.y )
    static member inline (-) (a: Vector2, b: float32) = Vector2( a.x - b, a.y - b )
    static member inline (-) (a: float32, b: Vector2) = Vector2( a - b.x, a - b.y )
    static member inline (-=) (a: Vector2, b: Vector2) = a.Set( a.x - b.x, a.y - b.y )
    static member inline (-=) (a: Vector2, b: float32) = a.Set( a.x - b, a.y - b )

    static member inline (*) (a: Vector2, b: Vector2) = Vector2( a.x * b.x, a.y * b.y )
    static member inline (*) (a: Vector2, b: float32) = Vector2( a.x * b, a.y * b )
    static member inline (*) (a: float32, b: Vector2) = Vector2( a * b.x, a * b.y )
    static member inline ( *=) (a: Vector2, b: Vector2) = a.Set( a.x * b.x, a.y * b.y )
    static member inline ( *=) (a: Vector2, b: float32) = a.Set( a.x * b, a.y * b )

    static member inline (/) (a: Vector2, b: Vector2) = Vector2( a.x / b.x, a.y / b.y )
    static member inline (/) (a: Vector2, b: float32) = Vector2( a.x / b, a.y / b )
    static member inline (/) (a: float32, b: Vector2) = Vector2( a / b.x, a / b.y )
    static member inline (/=) (a: Vector2, b: Vector2) = a.Set( a.x / b.x, a.y / b.y )
    static member inline (/=) (a: Vector2, b: float32) = a.Set( a.x / b, a.y / b )

    member inline a.Dot( b: Vector2 ) = a.x * b.x + a.y * b.y
    member inline a.Perp() = Vector2( -a.y, a.x )
    member inline a.SqMag() = a.Dot( a )
    member inline a.Mag() = sqrt (a.Dot( a ))
    member inline a.Macc( b: Vector2, s ) = a.Set( a.x + b.x * s, a.y + b.y * s )
    member inline a.Normal() = a / a.Mag()
    member inline a.Normalize() = a /= a.Mag()

    member inline a.ToArray() = [| a.x; a.y |]

and Vector3 =
    val mutable x: float32
    val mutable y: float32
    val mutable z: float32

    new( x, y, z ) = { x = x; y = y; z = z }
    new( x, y ) = { x = x; y = y; z = 0.0f }
    new( x ) = { x = x; y = 0.0f; z = 0.0f }

    member inline a.Set( x', y', z' ) = a.x <- x'; a.y <- y'; a.z <- z'
    member inline a.Set( b: Vector3 ) = a.x <- b.x; a.y <- b.y; a.z <- b.z
    member inline a.Set( v: float32[] ) = a.x <- v.[0]; a.y <- v.[1] ; a.z <- v.[2]

    static member inline (+) (a: Vector3, b: Vector3) = Vector3( a.x + b.x, a.y + b.y, a.z + b.z )
    static member inline (+) (a: Vector3, b: float32) = Vector3( a.x + b, a.y + b, a.z + b )
    static member inline (+) (a: float32, b: Vector3) = Vector3( a + b.x, a + b.y, a + b.z)
    static member inline (+=) (a: Vector3, b: Vector3) = a.Set( a.x + b.x, a.y + b.y, a.z + b.z )
    static member inline (+=) (a: Vector3, b: float32) = a.Set( a.x + b, a.y + b, a.z + b )

    static member inline (~-) (a: Vector3) = Vector3( -a.x, -a.y )
    static member inline (-) (a: Vector3, b: Vector3) = Vector3( a.x - b.x, a.y - b.y )
    static member inline (-) (a: Vector3, b: float32) = Vector3( a.x - b, a.y - b )
    static member inline (-) (a: float32, b: Vector3) = Vector3( a - b.x, a - b.y )
    static member inline (-=) (a: Vector3, b: Vector3) = a.Set( a.x - b.x, a.y - b.y, a.z - b.z )
    static member inline (-=) (a: Vector3, b: float32) = a.Set( a.x - b, a.y - b, a.z - b )

    static member inline (*) (a: Vector3, b: Vector3) = Vector3( a.x * b.x, a.y * b.y )
    static member inline (*) (a: Vector3, b: float32) = Vector3( a.x * b, a.y * b )
    static member inline (*) (a: float32, b: Vector3) = Vector3( a * b.x, a * b.y )
    static member inline ( *=) (a: Vector3, b: Vector3) = a.Set( a.x * b.x, a.y * b.y, a.z * b.z )
    static member inline ( *=) (a: Vector3, b: float32) = a.Set( a.x * b, a.y * b, a.z * b )

    static member inline (/) (a: Vector3, b: Vector3) = Vector3( a.x / b.x, a.y / b.y )
    static member inline (/) (a: Vector3, b: float32) = Vector3( a.x / b, a.y / b )
    static member inline (/) (a: float32, b: Vector3) = Vector3( a / b.x, a / b.y )
    static member inline (/=) (a: Vector3, b: Vector3) = a.Set( a.x / b.x, a.y / b.y, a.z / b.z )
    static member inline (/=) (a: Vector3, b: float32) = a.Set( a.x / b, a.y / b, a.z / b )

    member inline a.Dot( b: Vector3 ) = a.x * b.x + a.y * b.y + a.z *b.z
    member inline a.SqMag() = a.Dot( a )
    member inline a.Mag() = sqrt (a.Dot( a ))
    member inline a.Macc( b: Vector3, s ) = a.Set( a.x + b.x * s, a.y + b.y * s, a.z + b.z * s )
    member inline a.Normal() = a / a.Mag()
    member inline a.Normalize() = a /= a.Mag()
    member inline a.Cross( b: Vector3 ) = Vector3( a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x )

    member inline a.ToArray() = [| a.x; a.y; a.z |]
