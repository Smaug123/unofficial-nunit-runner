namespace TestRunner

open System
open WoofWare.DotnetRuntimeLocator
open System.IO
open System.Reflection
open System.Runtime.Loader

// Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
// Set AppContext.BaseDirectory to where the test DLL is.
// (This tells the DLL loader to look next to the test DLL for dependencies.)
type SetBaseDir (testDll : FileInfo) =
    let oldBaseDir = AppContext.BaseDirectory
    do AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)

    interface IDisposable with
        member _.Dispose () =
            AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)


type Ctx (dll : FileInfo, runtimes : DirectoryInfo list) =
    inherit AssemblyLoadContext ()

    override this.Load (target : AssemblyName) : Assembly =
        let path = Path.Combine (dll.Directory.FullName, $"%s{target.Name}.dll")

        if File.Exists path then
            this.LoadFromAssemblyPath path
        else

        runtimes
        |> List.tryPick (fun di ->
            let path = Path.Combine (di.FullName, $"%s{target.Name}.dll")

            if File.Exists path then
                this.LoadFromAssemblyPath path |> Some
            else
                None
        )
        |> Option.defaultValue null


module Program =
    let selectRuntime
        (config : RuntimeOptions)
        (f : DotnetEnvironmentInfo)
        : Choice<DotnetEnvironmentFrameworkInfo, DotnetEnvironmentSdkInfo> option
        =
        let rollForward =
            match Environment.GetEnvironmentVariable "DOTNET_ROLL_FORWARD" with
            | null ->
                config.RollForward
                |> Option.map RollForward.Parse
                |> Option.defaultValue RollForward.Minor
            | s -> RollForward.Parse s

        let desired = Version config.Framework.Version

        match rollForward with
        | RollForward.Minor ->
            let available =
                f.Frameworks
                |> Seq.choose (fun fi ->
                    if fi.Name = config.Framework.Name then
                        Some (fi, Version fi.Version)
                    else
                        None
                )
                |> Seq.filter (fun (_, version) -> version.Major = desired.Major && version.Minor >= desired.Minor)
                |> Seq.tryMinBy (fun (_, version) -> version.Minor, version.Build)

            match available with
            | Some (f, _) -> Some (Choice1Of2 f)
            | None ->
                // TODO: maybe we can ask the SDK. But we keep on trucking: maybe we're self-contained,
                // and we'll actually find all the runtime next to the DLL.
                None
        | _ -> failwith "non-minor RollForward not supported yet; please shout if you want it"

    let locateRuntimes (dll : FileInfo) : DirectoryInfo list =
        let runtimeConfig =
            let name =
                if not (dll.Name.EndsWith (".dll", StringComparison.OrdinalIgnoreCase)) then
                    failwith $"Expected DLL %s{dll.FullName} to end in .dll"

                dll.Name.Substring (0, dll.Name.Length - 4)

            Path.Combine (dll.Directory.FullName, $"%s{name}.runtimeconfig.json")
            |> File.ReadAllText
            |> System.Text.Json.Nodes.JsonNode.Parse
            |> RuntimeConfig.jsonParse
            |> fun f -> f.RuntimeOptions

        let availableRuntimes = DotnetEnvironmentInfo.Get ()

        let runtime = selectRuntime runtimeConfig availableRuntimes

        match runtime with
        | None ->
            // Keep on trucking: let's be optimistic and hope that we're self-contained.
            [ dll.Directory ]
        | Some (Choice1Of2 runtime) -> [ dll.Directory ; DirectoryInfo runtime.Path ]
        | Some (Choice2Of2 sdk) -> [ dll.Directory ; DirectoryInfo sdk.Path ]

    let main argv =
        let testDll, filter =
            match argv |> List.ofSeq with
            | [ dll ] -> FileInfo dll, None
            | [ dll ; "--filter" ; filter ] -> FileInfo dll, Some (Filter.parse filter)
            | _ -> failwith "provide exactly one arg, a test DLL"

        let filter =
            match filter with
            | Some filter -> Filter.shouldRun filter
            | None -> fun _ _ -> true

        let progress = Progress.spectre ()

        use _ = new SetBaseDir (testDll)

        let ctx = Ctx (testDll, locateRuntimes testDll)
        let assy = ctx.LoadFromAssemblyPath testDll.FullName

        let anyFailures =
            assy.ExportedTypes
            // TODO: NUnit nowadays doesn't care if you're a TestFixture or not
            |> Seq.filter (fun ty ->
                ty.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TestFixtureAttribute")
            )
            |> Seq.fold
                (fun anyFailures ty ->
                    let testFixture = TestFixture.parse ty

                    let results = TestFixture.run progress filter testFixture

                    let anyFailures =
                        match results.Failed with
                        | [] -> anyFailures
                        | _ :: _ ->
                            eprintfn $"%i{results.Failed.Length} tests failed"
                            true

                    let anyFailures =
                        match results.OtherFailures with
                        | [] -> anyFailures
                        | otherFailures ->
                            eprintfn "Other failures encountered: "

                            for failure in otherFailures do
                                eprintfn $"  %s{failure.Name}"

                            true

                    anyFailures
                )
                false

        if anyFailures then 1 else 0

    [<EntryPoint>]
    let reallyMain argv =
        // Hack to make sure `finally`s get run.
        // (The runtime doesn't define which `finally`s, if any, run when an uncaught exception terminates execution.)
        try
            main argv
        with _ ->
            reraise ()
