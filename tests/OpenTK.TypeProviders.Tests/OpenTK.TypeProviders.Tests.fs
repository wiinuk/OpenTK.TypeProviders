module OpenTK.TypeProviders.Tests
open NUnit.Framework
open OpenTK.TypeProviders
open OpenTK.FSharp.OpenGL
open OpenTK.Graphics.OpenGL
open OpenTK


let test() =
    let x = new ShaderProgram<"attribute float v1; uniform vec4 v2; void main(void) { gl_Position = vec4(0.0); }", "void main(void) { gl_FlagColor = vec4(1.0); }">()

    let (ShaderAttribute v1): float32 ShaderAttribute = x.Variables.v1
    Assert.AreEqual(typeof<float32>.TypeHandle, v1.definition.cliType)
    Assert.AreEqual("float", v1.definition.meta.openGLTypeName)
    Assert.AreEqual("v1", v1.definition.meta.variableName)
    Assert.AreEqual(1, v1.definition.spec.vartexElementCount)
    Assert.AreEqual(VertexAttribPointerType.Float, v1.definition.spec.vertexType)

    let (ShaderUniform v2): Vector4 ShaderUniform = x.Variables.v2
    ()
