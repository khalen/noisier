module Camera

open FsAlg.Generic
open BaseTypes
open Nlib.Math

type Camera() =
    inherit Transform()

    member val Fov  = 2.5f

    member c.ToCameraSpace (v: Vec) = 
        c.Axis.Xform v


