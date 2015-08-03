module BaseTypes

open FsAlg.Generic
open Math

type Vec = Vector<float32>
type Mat = Matrix<float32>

type ITransform =
    abstract member Position : Vec with get, set
    abstract member Orientation : Mat with get, set
    abstract member Mat : Mat with get, set

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
            let right = (fwd %* (vec3 0.0f 1.0f 0.0f)).GetUnitVector()
            let up = right %* fwd
            q.up <- up; q.right <- right; q.fwd <- fwd

        new() = { up = vec3 0.0f 1.0f 0.0f; right = vec3 1.0f 0.0f 0.0f; fwd = vec3 0.0f 0.0f 1.0f }


type Transform( origin: Vec, orientation: Axis ) =
    let mutable pos = origin
    let mutable axis = orientation

    new() = Transform( vec3 0.0f 0.0f 0.0f, mat3x3identity )

    member __.MakeClean() =
        if dirty then do
            dirty <- false
            let xlat = mat3x4CreateTranslation pos
            let rot = mat3x3to3x4 axis
            mat3x4 <- rot * xlat
            invMat <- mat3x4.GetInverse()

    member v.InverseMat 
        with get() = v.MakeClean(); invMat

    member v.InverseOrientation
        with get() = v.MakeClean(); invMat.[0..2, 0..2]

    member v.Up with get() = axis.row

    interface ITransform with
        member v.Position
            with get() = v.MakeClean(); pos
            and set newPos = dirty <- true; pos <- newPos
        member v.Orientation
            with get() = v.MakeClean(); axis
            and set newAxis = dirty <- true; axis <- newAxis
        member v.Mat
            with get() = v.MakeClean(); mat3x4
            and set newMat =
                dirty <- false
                pos <- vector (Matrix.row 3 newMat |> Matrix.toArray)
                axis <- newMat.[0..2,0..2]
    
    member v.LookAt( origin, target ) =
        let lookatMtx = mat3x4CreateLookat origin target 0.0f
        (v :> ITransform).Mat <- lookatMtx

    member v.LookAt( target ) =
        v.LookAt( pos, target )
