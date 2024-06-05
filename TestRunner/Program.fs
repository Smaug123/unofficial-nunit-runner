namespace TestRunner

open System
open System.IO
open System.Reflection

// Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
// Set AppContext.BaseDirectory to where the test DLL is.
// (This tells the DLL loader to look next to the test DLL for dependencies.)
type SetBaseDir (testDll : FileInfo) =
    let oldBaseDir = AppContext.BaseDirectory
    do AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)

    interface IDisposable with
        member _.Dispose () =
            AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)

module Program =
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

        let progress = TestProgress.toStderr ()

        use _ = new SetBaseDir (testDll)

        let assy = Assembly.LoadFrom testDll.FullName

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
