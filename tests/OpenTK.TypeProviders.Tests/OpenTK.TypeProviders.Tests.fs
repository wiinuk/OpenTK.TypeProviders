module OpenTK.TypeProviders.Tests
open NUnit.Framework
open OpenTK.TypeProviders
open OpenTK.FSharp.OpenGL

let test() =
    let x = new ShaderProgram<"attribute float v1; uniform vec4 v2; void main(void) { gl_Position = vec4(0.0); }", "void main(void) { gl_FlagColor = vec4(1.0); }">()

    let v1: float ShaderAttribute = x.Variables.v1
    v1
