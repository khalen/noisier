// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Noise

[<System.STAThread; EntryPoint>]
let main argv = 
    testPerlin()
    0 // return an integer exit code
