module BaseTypes

open FsAlg.Generic

type Vec = Vector<float32>
type Mat = Matrix<float32>

type ITransform =
    abstract member Pos : Vec with get, set
    abstract member Mat : Mat with get, set

