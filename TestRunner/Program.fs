namespace TestRunner

open System
open System.IO
open System.Reflection

module Program =
    let main argv =
        let testDll, filter =
            match argv |> List.ofSeq with
            | [ dll ] -> FileInfo dll, None
            | [ dll ; "--filter" ; filter ] -> FileInfo dll, Some (FilterIntermediate.parse filter |> Filter.make)
            | _ -> failwith "provide exactly one arg, a test DLL"

        let filter =
            match filter with
            | Some filter -> TestFixture.shouldRun filter
            | None -> fun _ _ -> true

        // Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
        // Set AppContext.BaseDirectory to where the test DLL is.
        // (This tells the DLL loader to look next to the test DLL for dependencies.)
        let oldBaseDir = AppContext.BaseDirectory
        AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)
        let assy = Assembly.LoadFrom testDll.FullName

        let anyFailures =
            try
                assy.ExportedTypes
                // TODO: NUnit nowadays doesn't care if you're a TestFixture or not
                |> Seq.filter (fun ty ->
                    ty.CustomAttributes
                    |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TestFixtureAttribute")
                )
                |> Seq.fold
                    (fun anyFailures ty ->
                        let testFixture = TestFixture.parse ty

                        match TestFixture.run filter testFixture with
                        | 0 -> anyFailures
                        | i ->
                            eprintfn $"%i{i} tests failed"
                            true
                    )
                    false
            finally
                AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)

        if anyFailures then 1 else 0

    [<EntryPoint>]
    let reallyMain argv =
        // Hack to make sure `finally`s get run.
        // (The runtime doesn't define which `finally`s, if any, run when an uncaught exception terminates execution.)
        try
            main argv
        with _ ->
            reraise ()
