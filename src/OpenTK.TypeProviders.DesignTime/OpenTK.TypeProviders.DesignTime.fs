module OpenTK.TypeProviders.Implementation
open System
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open OpenTK.FSharp.OpenGL
open OpenTK.FSharp.Shaders.Ast


[<AutoOpen>]
module internal Helpers =
    let rec tryPick picker e =
        match picker e with
        | Some _ as r -> r
        | _ ->

        match e with
        | ExprShape.ShapeCombination(_, es) -> List.tryPick picker es
        | ExprShape.ShapeLambda(_, e) -> tryPick picker e
        | ExprShape.ShapeVar _ -> None

    let findMethod e =
        e
        |> tryPick (function
            | Patterns.Call(_, m, _) ->
                if m.IsGenericMethod then Some(m.GetGenericMethodDefinition(), m.GetGenericArguments())
                else Some(m, Array.empty)
            | _ -> None
        )
        |> Option.defaultWith (fun _ -> failwithf "e.g. <@ f(x) @>")

    let readIfPathLike currentDirectory pathOrSource =
        if Uri.IsWellFormedUriString(pathOrSource, UriKind.RelativeOrAbsolute) then
            let path = Path.Combine(currentDirectory, pathOrSource)
            { source = File.ReadAllText path; name = path }
        else
            { source = pathOrSource; name = "" }

    let makeShaderVariableMetadataExpr { variableName = name; openGLTypeName = openGLTypeName } =
        <@ { variableName = name; openGLTypeName = openGLTypeName } @>

    let makeShaderVariableSpecExpr = function
        | ShaderAttributeSpec { vartexElementCount = vartexElementCount; vertexType = vertexType } ->
            <@
                ShaderAttributeSpec {
                    vartexElementCount = vartexElementCount
                    vertexType = vertexType
                }
            @>
        | ShaderUniformSpec ShaderUniformSpec.ShaderUniformSpec ->
            <@
                ShaderUniformSpec ShaderUniformSpec.ShaderUniformSpec
            @>

    let makeArrayExpr (es: 'T Expr seq) =
        let es = es |> Seq.map (fun e -> e.Raw) |> Seq.toList
        <@ %%(Expr.NewArray(typeof<'T>, es)): 'T array @>

    let makeVariableDefinitionsExpr variableDefinitions =
        let typeofMD, _ = findMethod <@ typeof<_> @>
        variableDefinitions
        |> Seq.map (fun d ->
            let typeofM = typeofMD.MakeGenericMethod(Type.GetTypeFromHandle d.cliType)
            let typeofCliType = <@ %%(Expr.Call(typeofM, [])): Type @>
            <@
            {
                meta = %(makeShaderVariableMetadataExpr d.meta)
                cliType = (%typeofCliType).TypeHandle
                spec = %(makeShaderVariableSpecExpr d.spec)
            }
            @>
        )
        |> makeArrayExpr

[<TypeProvider>]
type public ShaderProgramProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, assemblyReplacementMap = ["OpenTK.TypeProviders.DesignTime", "OpenTK.TypeProviders.Runtime"])

    let ns = "OpenTK.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()

    // type ShaderProgram<Vertex: string, Fragment: string> =
    let shaderProgramT = ProvidedTypeDefinition(asm, ns, "ShaderProgram", Some typeof<obj>, hideObjectMethods = true, nonNullable = true)
    let vertexP = ProvidedStaticParameter("Vertex", typeof<string>)
    let fragmentP = ProvidedStaticParameter("Fragment", typeof<string>)
    do shaderProgramT.DefineStaticParameters([vertexP; fragmentP], fun providedTypeName args ->
        let vertex = args.[0] :?> string
        let fragment = args.[1] :?> string

        // nested type ShaderVariables =
        //     inherit ShaderVariable array
        let variablesT = ProvidedTypeDefinition("ShaderVariables", Some typeof<ShaderVariable array>)

        let currentFolder = config.ResolutionFolder
        let { source = vertexSource } as vertex = readIfPathLike currentFolder vertex
        let { source = fragmentSource } as fragment = readIfPathLike currentFolder fragment
        let sourceInfos = [vertex; fragment] |> Seq.collect SimpleParser.parseSimpleSourceInfo |> Seq.cache

        let getAttributeMD, _ = findMethod <@ Untype.getAttribute<_> @>
        let getUniformMD, _ = findMethod <@ Untype.getUniform<_> @>

        sourceInfos
        |> Seq.mapi (fun i { location = l; value = d } ->
            let propertyType, getM =
                let t = Type.GetTypeFromHandle d.cliType

                match d.spec with
                | ShaderAttributeSpec _ ->
                    let propertyType = typedefof<ShaderAttribute<_>>.MakeGenericType(t)
                    let m = getAttributeMD.MakeGenericMethod t
                    propertyType, m

                | ShaderUniformSpec _ ->
                    let propertyType = typedefof<ShaderUniform<_>>.MakeGenericType(t)
                    let m = getUniformMD.MakeGenericMethod t
                    propertyType, m

            // member vs.%(d.meta.variableName): %(d.cliType) ShaderAttribute = getAsAttribute<%(d.cliType)> vs %i
            // member vs.%(d.meta.variableName): %(d.cliType) ShaderUniform = getAsUniform<%(d.cliType)> vs %i

            // TODO: mangle, escape
            let p =
                ProvidedProperty(d.meta.variableName, propertyType, getterCode =
                    fun args -> Expr.Call(getM, [ <@ %%(args.[0]): ShaderVariable array @>; <@ i @> ])
                )

            do
                let kind =
                    match d.spec with
                    | ShaderAttributeSpec _ -> "attribute"
                    | ShaderUniformSpec _ -> "uniform"

                let locationString =
                    match l with
                    | None -> ""
                    | Some l -> sprintf " from %s:(%d,%d-%d,%d)" l.name l.position1.line l.position1.column l.position2.line l.position2.column

                p.AddXmlDoc <| sprintf "`%s %s %s;`%s" kind d.meta.openGLTypeName d.meta.variableName locationString

            match l with
            | None -> ()
            | Some { position1 = p1; name = maybeFilePath } ->
                p.AddDefinitionLocation(p1.line, p1.column, maybeFilePath)

            p
        )
        |> Seq.toList
        |> variablesT.AddMembers

        // type %providedTypeName =
        let providedShaderProgramT = ProvidedTypeDefinition(asm, ns, providedTypeName, Some typeof<Untype.ShaderProgram>)

        //     new () = ShaderProgram(%vertexSource, %fragmentSource, %variableDefinitions)
        do
            ProvidedConstructor([], 
                invokeCode = fun _ ->
                let makeVariableDefinitions =
                    sourceInfos
                    |> Seq.map (fun x -> x.value)
                    |> makeVariableDefinitionsExpr
                <@@
                    Untype.ShaderProgram(vertexSource, fragmentSource, %makeVariableDefinitions)
                @@>
            )
            |> providedShaderProgramT.AddMember

        //     member program.Variables: %providedTypeName.ShaderVariables = program.Variables
        do
            ProvidedProperty("Variables", variablesT, 
                getterCode = fun args -> <@@ (%%args.[0]: Untype.ShaderProgram).Variables @@>
            )
            |> providedShaderProgramT.AddMember

        providedShaderProgramT.AddMember variablesT
        providedShaderProgramT
    )

    do this.AddNamespace(ns, [shaderProgramT])


[<TypeProviderAssembly>]
do ()
