namespace OpenTK.FSharp.OpenGL
open System
open OpenTK.Graphics.OpenGL


type IBufferPhantom<'T> = interface end
[<AbstractClass; Sealed>]
type ElementArrayBufferPhantom<'T> =
    interface IBufferPhantom<'T>

[<AbstractClass; Sealed>]
type ArrayBufferPhantom<'T> =
    interface IBufferPhantom<'T>

type ITexturePhantom = interface end
[<AbstractClass; Sealed>]
type Texture2DPhantom =
    interface ITexturePhantom

[<AbstractClass; Sealed>]
type ProgramPhantom = class end

[<Struct>]
type Handle<'Phantom> = Handle of int

[<Struct>]
type BufferInfo<'T> = {
    handle: 'T Handle
    length: int
}

[<Struct>]
type ShaderAttributeSpec = {
    vertexType: VertexAttribPointerType
    vartexElementCount: int
}
type ShaderUniformSpec = | ShaderUniformSpec

type ShaderVariableMetadata = {
    openGLTypeName: string
    variableName: string
}

[<Struct>]
type ShaderVariableDefinition<'Spec> = {
    cliType: RuntimeTypeHandle
    meta: ShaderVariableMetadata
    spec: 'Spec
}

[<Struct>]
type ShaderVariable<'Definition> = {
    location: int
    definition: 'Definition
}

type ShaderAttribute = ShaderAttributeSpec ShaderVariableDefinition ShaderVariable
type ShaderUniform = ShaderUniformSpec ShaderVariableDefinition ShaderVariable

type ShaderVariableSpec =
    | ShaderAttributeSpec of ShaderAttributeSpec
    | ShaderUniformSpec of ShaderUniformSpec

type ShaderVariable =
    | ShaderAttribute of ShaderAttribute
    | ShaderUniform of ShaderUniform

[<Struct>]
type ShaderAttribute<'T> = ShaderAttribute of ShaderAttribute
[<Struct>]
type ShaderUniform<'T> = ShaderUniform of ShaderUniform

[<AutoOpen>]
module internal Internals =
    let glfailwithf format =
        Printf.ksprintf (fun m ->
            let code =
                match GL.GetError() with
                | ErrorCode.NoError -> ""
                | c -> sprintf "\nerror code: %A" c
    
            failwithf "%s%s" m code
        ) format

module GL =
    open System.IO
    open System.Drawing
    open System.Drawing.Imaging
    open OpenTK


    let buildProgram vertexShaderSource fragmentShaderSource: ProgramPhantom Handle =
        let compileShader source shaderType =
            let shader = GL.CreateShader shaderType
            if shader <= 0 then glfailwithf "CreateShader() = %d" shader

            // TODO: 非 ascii 文字をエスケープ
            let source =
                source
                |> String.collect (function
                    | '\\' -> "\\\\"
                    | c when '\127' < c -> sprintf "\\u%04X" (int c)
                    | c -> string c
                )

            GL.ShaderSource(shader, source)
            GL.CompileShader shader
            if GL.GetShader(shader, ShaderParameter.CompileStatus) = 0 then
                glfailwithf "%s\n%A\n%s" (GL.GetShaderInfoLog shader) shaderType source

            shader

        let v = compileShader vertexShaderSource ShaderType.VertexShader
        let f = compileShader fragmentShaderSource ShaderType.FragmentShader
        let program = GL.CreateProgram()
        if program <= 0 then glfailwithf "CreateProgram() = %d" program

        GL.AttachShader(program, v)
        GL.AttachShader(program, f)

        GL.DeleteShader v
        GL.DeleteShader f

        GL.LinkProgram program
        if GL.GetProgram(program, GetProgramParameterName.LinkStatus) = 0 then
            glfailwithf "%s" <| GL.GetProgramInfoLog program

        GL.UseProgram program
        Handle program

    let private initializeBufferCore target (data: 'T array): #IBufferPhantom<'T> BufferInfo =
        let buffer = GL.GenBuffer()
        if buffer <= 0 then glfailwithf "GenBuffer() = %d" buffer

        GL.BindBuffer(target, buffer)
        GL.BufferData(target, sizeof<'T> * data.Length, &data.[0], BufferUsageHint.StaticDraw)
        GL.BindBuffer(target, 0)
        { handle = Handle buffer; length = data.Length }

    let initializeBuffer data: _ ArrayBufferPhantom BufferInfo =
        initializeBufferCore BufferTarget.ArrayBuffer data

    let initializeElementBuffer data: _ ElementArrayBufferPhantom BufferInfo =
        initializeBufferCore BufferTarget.ElementArrayBuffer data

    let deleteBuffer ({ handle = Handle h }: #IBufferPhantom<_> BufferInfo) = GL.DeleteBuffer h
    let deleteTexture (Handle h: #ITexturePhantom Handle) = GL.DeleteTexture h

    let loadAndCreateTexture (dataStream: Stream): Texture2DPhantom Handle =
        use bitmap = new Bitmap(dataStream)

        // png の反転を直す
        bitmap.RotateFlip RotateFlipType.RotateNoneFlipY
        let data = bitmap.LockBits(Rectangle(0, 0, bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        try
            let texture = GL.GenTexture()
            if texture <= 0 then glfailwithf "GenTexture() = %d" texture

            GL.BindTexture(TextureTarget.Texture2D, texture)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)

            let mipmapLevel = 0
            let borderWidth = 0
            GL.TexImage2D(
                TextureTarget.Texture2D,
                mipmapLevel,
                PixelInternalFormat.Rgba,
                data.Width,
                data.Height,
                borderWidth,
                PixelFormat.Rgba,
                PixelType.UnsignedByte,
                data.Scan0
            )
            GL.BindTexture(TextureTarget.Texture2D, 0)
            Handle texture

        finally
            bitmap.UnlockBits data

    let deleteProgram (Handle h: ProgramPhantom Handle) =
        GL.DeleteProgram h

    let bindTexture (Handle h: Texture2DPhantom Handle) =
        GL.BindTexture(TextureTarget.Texture2D, h)

    let uniform1I (ShaderUniform a: int ShaderUniform) (v: int) =
        GL.Uniform1(a.location, v)

    let uniform1F (ShaderUniform a: single ShaderUniform) (v: single) =
        GL.Uniform1(a.location, v)

    let uniformMatrix4 (ShaderUniform a: Matrix4 ShaderUniform) (matrix: Matrix4 byref) =
        GL.UniformMatrix4(a.location, false, &matrix)

    let uniformVector4 (ShaderUniform a: Vector4 ShaderUniform) (v: Vector4) =
        GL.Uniform4(a.location, v)

    let bindBuffer (ShaderAttribute a: 'T ShaderAttribute) ({ handle = Handle h }: #IBufferPhantom<'T> BufferInfo) =
        GL.BindBuffer(BufferTarget.ArrayBuffer, h)
        GL.EnableVertexAttribArray a.location
        GL.VertexAttribPointer(a.location, a.definition.spec.vartexElementCount, a.definition.spec.vertexType, false, 0, 0)

    let bindElementBuffer ({ handle = Handle h }: 'T ElementArrayBufferPhantom BufferInfo) =
        GL.BindBuffer(BufferTarget.ElementArrayBuffer, h)

    let drawElements (b: 'T ElementArrayBufferPhantom BufferInfo) =
        GL.DrawElements(PrimitiveType.Triangles, b.length, DrawElementsType.UnsignedShort, 0)

[<Struct>]
type ShaderVariables =
    val private variables: ShaderVariable array
    new (program: ProgramPhantom Handle, variableDefinitions) = {
        variables =
            let (Handle program) = program

            variableDefinitions
            |> Seq.map (fun d ->
                match d.spec with
                | ShaderAttributeSpec spec ->
                    ShaderVariable.ShaderAttribute {
                        location = GL.GetAttribLocation(program, d.meta.variableName)
                        definition = { cliType = d.cliType; meta = d.meta; spec = spec }
                    }
                | ShaderUniformSpec spec ->
                    ShaderVariable.ShaderUniform {
                        location = GL.GetUniformLocation(program, d.meta.variableName)
                        definition = { cliType = d.cliType; meta = d.meta; spec = spec }
                    }
            )
            |> Seq.toArray
    }
    member vs.Item with get index = vs.variables.[index]
    member vs.All() = vs.variables |> Seq.readonly
    
    [<RequiresExplicitTypeArguments>]
    member vs.GetAttribute<'T> index: 'T ShaderAttribute =
        match vs.[index] with
        | ShaderVariable.ShaderAttribute v when typeof<'T>.TypeHandle = v.definition.cliType -> ShaderAttribute.ShaderAttribute v
        | v -> failwithf "attribute (%s) not found. found variable: %A" typeof<'T>.Name v

    [<RequiresExplicitTypeArguments>]
    member vs.GetUniform<'T> index: 'T ShaderUniform =
        match vs.[index] with
        | ShaderVariable.ShaderUniform v when typeof<'T>.TypeHandle = v.definition.cliType -> ShaderUniform.ShaderUniform v
        | v -> failwithf "uniform (%s) not found. found variable: %A" typeof<'T>.Name v

module Untype =
    type ShaderProgram(vertexShaderSource, fragmentShaderSource, variableDefinitions) =
        let program = GL.buildProgram vertexShaderSource fragmentShaderSource
        let variables = ShaderVariables(program, variableDefinitions)

        member __.Handle = program
        member __.Variables = variables

namespace OpenTK.FSharp.Shaders.Ast
open OpenTK.FSharp.OpenGL


type SourceInfo = {
    source: string
    name: string
}
type ShaderVariableDefinition = ShaderVariableSpec ShaderVariableDefinition

[<Struct>]
type Position = {
    line: int
    column: int
}
[<Struct>]
type Location = {
    name: string
    position1: Position
    position2: Position
}
type 'T Source = {
    location: Location option
    value: 'T
}

[<AutoOpen>]
module internal Internals =
    open System.Text.RegularExpressions
    open OpenTK
    type T = OpenTK.Graphics.OpenGL.VertexAttribPointerType


    let variableRegex = Regex @"\b(attribute|uniform|varying)\s+(\w+)\s+(\w+)\s*;"
    let a<'T> n vertexType vertexElementCount =
        let s = {
            vertexType = vertexType
            vartexElementCount = vertexElementCount
        }
        n, (typeof<'T>, s)


    let glPrimitiveTypeMap = Map [|
        a<single> "float" T.Float 1
        a<double> "double" T.Double 1
        a<int32> "int" T.Int 1
        a<uint32> "uint" T.UnsignedInt 1
        a<bool> "bool" T.UnsignedByte 1
        a<Vector2> "vec2" T.Float 2
        a<Vector3> "vec3" T.Float 3
        a<Vector4> "vec4" T.Float 4
        a<Vector2d> "dvec2" T.Double 2
        a<Vector3d> "dvec3" T.Double 3
        a<Vector4d> "dvec4" T.Double 4
        //BVEC2
        //BVEC3
        //BVEC4
        //IVEC2
        //IVEC3
        //IVEC4
        //UVEC2
        //UVEC3
        //UVEC4
        a<Matrix2> "mat2" T.Float 4
        a<Matrix3> "mat3" T.Float 9
        a<Matrix4> "mat4" T.Float 16
        //MAT2X2
        //MAT2X3
        //MAT2X4
        //MAT3X2
        //MAT3X3
        //MAT3X4
        //MAT4X2
        //MAT4X3
        //MAT4X4
        //DMAT2
        //DMAT3
        //DMAT4
        //DMAT2X2
        //DMAT2X3
        //DMAT2X4
        //DMAT3X2
        //DMAT3X3
        //DMAT3X4
        //DMAT4X2
        //DMAT4X3
        //DMAT4X4
        //ATOMIC_UINT
        a<int> "sampler2D" T.Int 1
        //SAMPLER3D
        //SAMPLERCUBE
        //SAMPLER2DSHADOW
        //SAMPLERCUBESHADOW
        //SAMPLER2DARRAY
        //SAMPLER2DARRAYSHADOW
        //SAMPLERCUBEARRAY
        //SAMPLERCUBEARRAYSHADOW
        //ISAMPLER2D
        //ISAMPLER3D
        //ISAMPLERCUBE
        //ISAMPLER2DARRAY
        //ISAMPLERCUBEARRAY
        //USAMPLER2D
        //USAMPLER3D
        //USAMPLERCUBE
        //USAMPLER2DARRAY
        //USAMPLERCUBEARRAY
        //SAMPLER1D
        //SAMPLER1DSHADOW
        //SAMPLER1DARRAY
        //SAMPLER1DARRAYSHADOW
        //ISAMPLER1D
        //ISAMPLER1DARRAY
        //USAMPLER1D
        //USAMPLER1DARRAY
        //SAMPLER2DRECT
        //SAMPLER2DRECTSHADOW
        //ISAMPLER2DRECT
        //USAMPLER2DRECT
        //SAMPLERBUFFER
        //ISAMPLERBUFFER
        //USAMPLERBUFFER
        //SAMPLER2DMS
        //ISAMPLER2DMS
        //USAMPLER2DMS
        //SAMPLER2DMSARRAY
        //ISAMPLER2DMSARRAY
        //USAMPLER2DMSARRAY
        //IMAGE2D
        //IIMAGE2D
        //UIMAGE2D
        //IMAGE3D
        //IIMAGE3D
        //UIMAGE3D
        //IMAGECUBE
        //IIMAGECUBE
        //UIMAGECUBE
        //IMAGEBUFFER
        //IIMAGEBUFFER
        //UIMAGEBUFFER
        //IMAGE1D
        //IIMAGE1D
        //UIMAGE1D
        //IMAGE1DARRAY
        //IIMAGE1DARRAY
        //UIMAGE1DARRAY
        //IMAGE2DRECT
        //IIMAGE2DRECT
        //UIMAGE2DRECT
        //IMAGE2DARRAY
        //IIMAGE2DARRAY
        //UIMAGE2DARRAY
        //IMAGECUBEARRAY
        //IIMAGECUBEARRAY
        //UIMAGECUBEARRAY
        //IMAGE2DMS
        //IIMAGE2DMS
        //UIMAGE2DMS
        //IMAGE2DMSARRAY
        //IIMAGE2DMSARRAY
        //UIMAGE2DMSARRAY
    |]
    let tryFindGLType typeSource =
        Map.tryFind typeSource glPrimitiveTypeMap

    /// liner search
    let indexToPosition (source: string) index =
        let rec aux l c i =
            if i < String.length source && i < index then
                if source.[i] = '\n' then
                    aux (l + 1) 1 (i + 1)
                else
                    aux l (c + 1) (i + 1)
            else { line = l; column = c }
        aux 1 1 0

module SimpleParser =
    let parseSimpleSourceInfo { name = path; source = source } =
        seq {
            for m in variableRegex.Matches source do
                let k = m.Groups.[1]
                let tn = m.Groups.[2]
                let n = m.Groups.[3]
                match tryFindGLType tn.Value with
                | None -> ()
                | Some(t, s) ->

                let s =
                    match k.Value with
                    | "attribute" -> Some <| ShaderAttributeSpec s
                    | "uniform" -> Some <| ShaderUniformSpec ShaderUniformSpec.ShaderUniformSpec
                    | _ -> None

                match s with
                | None -> ()
                | Some s ->

                let d = { cliType = t.TypeHandle; meta = { variableName = n.Value; openGLTypeName = tn.Value }; spec = s }

                let l = {
                    name = path
                    position1 = indexToPosition source m.Index
                    position2 = indexToPosition source (m.Index + m.Length)
                }
                yield { location = Some l; value = d }
        }

    let variableDefinitionsFromSource vertexSource fragmentSource =
        [vertexSource; fragmentSource]
        |> Seq.collect parseSimpleSourceInfo
        |> Seq.map (fun { value = v } -> v)

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("OpenTK.TypeProviders.DesignTime.dll")>]
do ()
