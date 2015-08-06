open Noise
open Raymarch
open Camera
open Nlib.Math
open System.Windows
open System.Windows.Media.Imaging

let heightFunctionConstant (_: float32) (_: float32) =
    0.0f
    
let heightFunctionNoise (nsrc: INoise) (extra: float32[]) x z =
    let fullPt = Array.concat [ [|x * 0.0032f + 30.0f; z * 0.0032f + 30.0f|]; extra ]
    max ((nsrc.GetValue( fullPt ).[0] * 0.5f + 0.5f) * 75.0f) 30.0f
    
[<System.STAThread; EntryPoint>]
let main argv = 
    let nx = 512
    let ny = 512
    let image = Controls.Image( Stretch = Media.Stretch.Uniform )    
    let timeLabel = Controls.Label()
    let format = Media.PixelFormats.Rgb128Float
    let pixels = Array.create<float32> (nx * ny * 4) 0.0f
    let panel = Controls.DockPanel()
    panel.HorizontalAlignment <- HorizontalAlignment.Stretch
    panel.VerticalAlignment <- VerticalAlignment.Stretch
    let add (ctrl: UIElement) =
      Controls.DockPanel.SetDock(ctrl, Controls.Dock.Top)
      panel.Children.Add ctrl |> ignore
    add timeLabel
    add image

    let cam = Camera()
    cam.LookAt( vec3 0.0f 60.0f 0.0f, vec3 0.0f 0.0f 200.0f )

    let heightFunc = heightFunctionNoise (new FBM()) [| 15.0f |] 
    // let heightFunc = heightFunctionConstant
    rayMarch cam heightFunc nx ny pixels
    image.Source <- BitmapSource.Create( nx, ny, 1.0, 1.0, format, null, pixels, nx * 16 )

    Window( Content = panel, Title = "Ray March" )
        |> (Application()).Run |> ignore

    0 // return an integer exit code
