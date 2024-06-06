namespace TestRunner

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Runtime.Loader
open System.Text.RegularExpressions

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
            let path = Path.Combine (dll.Directory.FullName, $"%s{target.Name}.dll")

            if File.Exists path then
                this.LoadFromAssemblyPath path |> Some
            else
                None
        )
        |> Option.defaultValue null

module Program =
    /// This is *the most cursed* method. There must surely be a better way.
    let locateRuntimes (dll : FileInfo) : DirectoryInfo list =
        let resolver =
            PathAssemblyResolver
                [|
                    yield dll.FullName
                    yield! Directory.GetFiles (RuntimeEnvironment.GetRuntimeDirectory (), "*.dll")
                    yield! Directory.GetFiles (dll.Directory.FullName, "*.dll")
                |]

        use mlc = new MetadataLoadContext (resolver)
        let assy = mlc.LoadFromAssemblyPath dll.FullName

        let runtime =
            assy.CustomAttributes
            |> Seq.find (fun att -> att.AttributeType.FullName = "System.Runtime.Versioning.TargetFrameworkAttribute")
            |> fun attr -> Seq.exactlyOne attr.ConstructorArguments
            |> fun args -> args.Value |> unbox<string>

        let regex = Regex "\\.NETCoreApp,Version=v([0-9]+)\.[0-9]+"
        let mat = regex.Match (runtime)

        if not mat.Success then
            failwith $"Could not identify runtime: %s{runtime}"

        let runtimeVersion = mat.Groups.[1].Value |> Int32.Parse

        let runtimes =
            let psi = ProcessStartInfo "dotnet"
            psi.ArgumentList.Add "--list-runtimes"
            psi.RedirectStandardOutput <- true
            use proc = new Process ()
            proc.StartInfo <- psi

            if not (proc.Start ()) then
                failwith "Could not start dotnet"

            let version = proc.StandardOutput.ReadToEnd ()
            proc.WaitForExit ()

            if proc.ExitCode <> 0 then
                failwith $"dotnet quit with exit code %i{proc.ExitCode}"

            version

        runtimes.Split ('\n')
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> Seq.choose (fun runtime ->
            let split = runtime.Split ' '
            let target = split.[1]

            if Int32.Parse (target.Split '.').[0] = runtimeVersion then
                let dir = split.[split.Length - 1]

                Path.Combine (dir.Substring (1, dir.Length - 2), target)
                |> DirectoryInfo
                |> Some
            else
                None
        )
        |> Seq.toList

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
