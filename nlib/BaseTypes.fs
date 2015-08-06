module BaseTypes

open FsAlg.Generic
open Nlib.Math

type Vec = Vector<float32>
type Mat = Matrix<float32>

type ITransform =
    abstract member Position : Vec with get, set
    abstract member Orientation : Mat with get, set

type IObject =
    abstract member DistanceTo : Vec -> float

type Axis = {
    mutable up: Vec
    mutable right: Vec
    mutable fwd: Vec
}
    with
        member q.Xform (vec: Vec) =
            vec3 (q.right * vec) (q.up * vec) (q.fwd * vec)

        member q.LookAt (origin: Vec) (dest: Vec) =
            let fwd = (dest - origin).GetUnitVector()
            let vertical = vec3 0.0f 1.0f 0.0f
            let right, up =
                if fwd * vertical < 0.0001f then
                    let up = (fwd %* vec3 1.0f 0.0f 0.0f).GetUnitVector()
                    let right = up %* fwd
                    (right, up)
                else
                    let right = (vec3 0.0f 1.0f 0.0f %* fwd).GetUnitVector()
                    let up = fwd %* right
                    (right, up)
            q.up <- up; q.right <- right; q.fwd <- fwd

        member q.ToMatrix() =
            matrix [| q.right.ToArray(); q.up.ToArray(); q.fwd.ToArray() |]

        member q.Orthonormalize() =
            q.fwd <- q.fwd.GetUnitVector()
            q.right <- (q.fwd %* q.up).GetUnitVector()
            q.up <- q.right %* q.fwd

        static member FromMatrix (mtx: Mat) =
            let axis = { right = mtx.[0,0..2] |> Matrix.toVector; up = mtx.[1,0..2] |> Matrix.toVector; fwd = mtx.[2,0..2] |> Matrix.toVector }
            axis.Orthonormalize()
            axis

let defaultAxis = { up = vec3 0.0f 1.0f 0.0f; right = vec3 1.0f 0.0f 0.0f; fwd = vec3 0.0f 0.0f 0.0f }

type Transform( origin: Vec, orientation: Axis ) =
    let mutable origin = origin
    let mutable axis   = orientation

    new() = Transform( vec3 0.0f 0.0f 0.0f, defaultAxis )

    member v.InverseOrientation
        with get() = axis.ToMatrix().GetInverse()

    member v.Up with get() = axis.up
    member v.Right with get() = axis.right
    member v.Fwd with get() = axis.fwd

    member __.Axis with get() = axis; and set newAxis = axis <- newAxis
    member __.Origin with get() = origin; and set newOrigin = origin <- newOrigin

    interface ITransform with
        member v.Position
            with get() = origin
            and set newPos = origin <- newPos
        member v.Orientation
            with get() = axis.ToMatrix()
            and set newMtx = axis <- Axis.FromMatrix newMtx
    
    member v.LookAt( newOrigin, target ) =
        origin <- newOrigin
        axis.LookAt newOrigin target

    member v.LookAt( target ) =
        axis.LookAt origin target
