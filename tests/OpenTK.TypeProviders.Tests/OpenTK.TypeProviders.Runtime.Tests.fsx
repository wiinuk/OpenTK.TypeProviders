#I "../../src/OpenTK.TypeProviders.Runtime/bin/Debug/net461/"
#r "./OpenTK.dll"
#r "./OpenTK.TypeProviders.Runtime.dll"


open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input
open OpenTK.TypeProviders
open OpenTK.FSharp.OpenGL


type TestWindow() =
    inherit GameWindow(480, 360)

    let program = new ShaderProgram<"./shader.vert.glsl", "./shader.flag.glsl">()
    let positions =
        GL.initializeBuffer [|
            Vector3(-1.f, 1.f, 0.f)
            Vector3(1.f, 1.f, 0.f)
            Vector3(-1.f, -1.f, 0.f)
            Vector3(1.f, -1.f, 0.f)
        |]

    let colors =
        GL.initializeBuffer [|
            Vector4(1.f, 0.f, 0.f, 1.f)
            Vector4(0.f, 1.f, 0.f, 1.f)
            Vector4(0.f, 0.f, 1.f, 1.f)
            Vector4(1.f, 1.f, 1.f, 1.f)
        |]

    let indexes =
        GL.initializeElementBuffer [|
            0us; 1us; 2us
            3us; 2us; 1us
        |]

    let mutable mvpMatrix = Matrix4.Identity

    let mutable isGrounded = false
    let mutable p = Vector2d.Zero
    let mutable v = Vector2d.Zero
    let accel norm = v <- v + Vector2d.Multiply(norm, 0.2)

    override __.OnLoad e =
        base.OnLoad e
        GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)
        GL.Enable EnableCap.DepthTest
        GL.Enable EnableCap.VertexArray
        GL.Enable EnableCap.IndexArray
        GL.EnableClientState ArrayCap.VertexArray
        GL.EnableClientState ArrayCap.IndexArray

    override __.OnKeyDown e =
        base.OnKeyDown e
        match e.Key with
        | Key.Right -> accel Vector2d.UnitX
        | Key.Left -> accel -Vector2d.UnitX
        | Key.Up ->
            if isGrounded then
                accel Vector2d.UnitY
                isGrounded <- false

        | Key.Down -> accel -Vector2d.UnitY
        | _ -> ()

    override g.OnKeyUp e =
        base.OnKeyUp e
        if e.Key = Key.Escape then g.Exit()

    override __.OnUpdateFrame e =
        base.OnUpdateFrame e

        p <- p + v
        if p.Y < 0. then
            isGrounded <- true
            v <- v + Vector2d(0., -p.Y)

        v <- v * 0.9
        v <- v + Vector2d(0., -0.01)

    override g.OnRenderFrame e =
        base.OnRenderFrame e

        mvpMatrix.Row3.Xy <- Vector2(single p.X, single p.Y)

        GL.ClearColor(Color4(0.f, 0.75f, 0.75f, 1.f))
        GL.ClearDepth 1.f
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

        GL.bindBuffer program.Variables.position positions
        GL.bindBuffer program.Variables.color colors
        GL.uniformMatrix4 program.Variables.mvpMatrix &mvpMatrix
        GL.bindElementBuffer indexes
        GL.drawElements indexes

        g.SwapBuffers()

do
    use w = new TestWindow()
    w.Run 60.
