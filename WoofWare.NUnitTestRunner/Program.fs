namespace WoofWare.NUnitTestRunner

open System
open System.IO
open System.Reflection
open System.Threading.Tasks
open Spectre.Console

type internal StartupHook () =
    static member public Initialize () : unit =
        Console.WriteLine "Hi!"

module Program =
    let main argv =
        let startTime = DateTimeOffset.Now

        let args = argv |> List.ofArray |> Args.Parse

        let filter =
            match args.Filter with
            | Some filter -> Filter.shouldRun filter
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

    [<EntryPoint>]
    let reallyMain argv =
        // Hack to make sure `finally`s get run.
        // (The runtime doesn't define which `finally`s, if any, run when an uncaught exception terminates execution.)
        try
            main argv
        with _ ->
            reraise ()
