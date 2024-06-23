namespace WoofWare.NUnitTestRunner

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading.Tasks
open Spectre.Console

module Program =
    // This is actually transcribed into C# in WoofWare.NUnitTestRunner.StartupHookLogic.
    let execute argv =
        let startTime = DateTimeOffset.Now

        let args = argv |> List.ofArray |> Args.Parse

        let filter =
            match args.Filter with
            | Some (_, filter) -> Filter.shouldRun filter
            | None -> fun _ _ -> true

        let stderr =
            let consoleSettings = AnsiConsoleSettings ()
            consoleSettings.Out <- AnsiConsoleOutput Console.Error
            AnsiConsole.Create consoleSettings

        let progress = Progress.spectre stderr

        let runtime = DotnetRuntime.locate args.Dll

        match args.Logging with
        | LogLevel.Nothing -> ()
        | LogLevel.Verbose ->
            for d in runtime do
                stderr.WriteLine $".NET runtime directory: %s{d.FullName}"

        use _ = new SetBaseDir (args.Dll)

        use contexts = TestContexts.Empty ()

        let ctx = LoadContext (args.Dll, runtime, contexts)
        let assy = ctx.LoadFromAssemblyPath args.Dll.FullName

        let attrs = AssemblyLevelAttributes.get assy

        let levelOfParallelism =
            match args.LevelOfParallelism, attrs.Parallelism with
            | None, None -> None
            | Some taken, Some ignored ->
                match args.Logging with
                | LogLevel.Nothing -> ()
                | LogLevel.Verbose ->
                    stderr.WriteLine
                        $"Taking parallelism %i{taken} from command line, ignoring value %i{ignored} from assembly"

                Some taken
            | Some x, None
            | None, Some x -> Some x

        let testFixtures = assy.ExportedTypes |> Seq.map TestFixture.parse |> Seq.toList

        use par = new ParallelQueue (levelOfParallelism, attrs.Parallelizable)

        let creationTime = DateTimeOffset.Now

        let results =
            testFixtures
            |> List.map (TestFixture.run contexts par progress filter)
            |> Task.WhenAll

        let timeout =
            match args.Timeout with
            | None -> TimeSpan.FromHours 2.0
            | Some t -> t

        if not (results.Wait timeout) then
            failwith "Tests failed to terminate within two hours"

        let results = results.Result |> Seq.concat |> List.ofSeq

        let report = BuildTrxReport.build assy creationTime startTime results

        match args.Trx with
        | Some trxPath ->
            let contents = TrxReport.toXml report |> fun d -> d.OuterXml
            trxPath.Directory.Create ()
            File.WriteAllText (trxPath.FullName, contents)
            Console.Error.WriteLine $"Written TRX file: %s{trxPath.FullName}"
        | None -> ()

        match report.ResultsSummary.Outcome with
        | TrxOutcome.Completed -> 0
        | _ -> 1

    let main argv =
        let args = argv |> List.ofArray |> Args.Parse

        let psi = ProcessStartInfo "dotnet"

        match args.Trx with
        | None -> ()
        | Some trx -> psi.EnvironmentVariables.Add ("WOOFWARE_NUNIT_GENERATE_TRX", trx.FullName)

        match args.LevelOfParallelism with
        | None -> ()
        | Some par -> psi.EnvironmentVariables.Add ("WOOFWARE_NUNIT_PARALLELISM_LEVEL", string<int> par)

        match args.Filter with
        | None -> ()
        | Some (filter, _) -> psi.EnvironmentVariables.Add ("WOOFWARE_NUNIT_FILTER", filter)

        match args.Timeout with
        | None -> ()
        | Some timeout ->
            psi.EnvironmentVariables.Add ("WOOFWARE_NUNIT_TIMEOUT_SECS", string<int> (int<float> timeout.TotalSeconds))

        psi.ArgumentList.Add "exec"
        psi.ArgumentList.Add args.Dll.FullName

        let us = Assembly.GetExecutingAssembly().Location |> FileInfo

        let startupHook =
            Path.Combine (us.Directory.FullName, "WoofWare.NUnitTestRunner.StartupHook.dll")

        psi.EnvironmentVariables.Add ("DOTNET_STARTUP_HOOKS", startupHook)

        psi.EnvironmentVariables.Add (
            "WOOFWARE_NUNIT_LIB",
            Path.Combine (us.Directory.FullName, "WoofWare.NUnitTestRunner.Lib.dll")
        )

        use proc = new Process ()
        proc.StartInfo <- psi

        if not (proc.Start ()) then
            failwith "Failed to start dotnet"

        proc.WaitForExit ()
        proc.ExitCode

    [<EntryPoint>]
    let reallyMain argv =
        // Hack to make sure `finally`s get run.
        // (The runtime doesn't define which `finally`s, if any, run when an uncaught exception terminates execution.)
        try
            main argv
        with _ ->
            reraise ()
