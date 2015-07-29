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

    new( x, y ) = { x = x; y = y }
    new( x ) = { x = x; y = x }
