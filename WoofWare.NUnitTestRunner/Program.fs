namespace WoofWare.NUnitTestRunner

open System
open System.IO
open System.Threading.Tasks
open Spectre.Console

// Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
// Set AppContext.BaseDirectory to where the test DLL is.
// (This tells the DLL loader to look next to the test DLL for dependencies.)
type SetBaseDir (testDll : FileInfo) =
    let oldBaseDir = AppContext.BaseDirectory
    do AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)

    interface IDisposable with
        member _.Dispose () =
            AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)

[<RequireQualifiedAccess>]
type LogLevel =
    | Nothing
    | Verbose

[<AutoOpen>]
module Patterns =
    let (|Key|_|) (start : string) (s : string) : string option =
        if s.StartsWith (start + "=", StringComparison.Ordinal) then
            s.Substring (start.Length + 1) |> Some
        else
            None

type Args =
    {
        Dll : FileInfo
        Trx : FileInfo option
        Filter : Filter option
        Logging : LogLevel
        LevelOfParallelism : int option
        Timeout : TimeSpan option
    }

    static member Parse (args : string list) : Args =
        match args with
        | [] -> failwith "The first arg must be a positional arg, the DLL to test."
        | dll :: args ->

        let rec go
            (trx : FileInfo option)
            (filter : Filter option)
            (logging : LogLevel option)
            (par : int option)
            (timeout : TimeSpan option)
            (args : string list)
            =
            match args with
            | [] ->
                {
                    Dll = FileInfo dll
                    Trx = trx
                    Filter = filter
                    Logging = logging |> Option.defaultValue LogLevel.Nothing
                    LevelOfParallelism = par
                    Timeout = timeout
                }
            | Key "--filter" filterStr :: rest
            | "--filter" :: filterStr :: rest ->
                match filter with
                | Some _ -> failwith "Two conflicting filters; you can only specify --filter once"
                | None -> go trx (Some (Filter.parse filterStr)) logging par timeout rest
            | Key "--trx" trxStr :: rest
            | "--trx" :: trxStr :: rest ->
                match trx with
                | Some _ -> failwith "Two conflicting TRX outputs; you can only specify --trx once"
                | None -> go (Some (FileInfo trxStr)) filter logging par timeout rest
            | Key "--verbose" verboseStr :: rest
            | "--verbose" :: verboseStr :: rest ->
                match logging with
                | Some _ -> failwith "Two conflicting --verbose outputs; you can only specify --verbose once"
                | None ->
                    let verbose =
                        if Boolean.Parse verboseStr then
                            LogLevel.Verbose
                        else
                            LogLevel.Nothing

                    go trx filter (Some verbose) par timeout rest
            | Key "--parallelism" parStr :: rest
            | "--parallelism" :: parStr :: rest ->
                match par with
                | Some _ -> failwith "Two conflicting --parallelism outputs; you can only specify --parallelism once"
                | None -> go trx filter logging (Some (Int32.Parse parStr)) timeout rest
            | Key "--timeout-seconds" timeoutStr :: rest
            | "--timeout-seconds" :: timeoutStr :: rest ->
                match timeout with
                | Some _ ->
                    failwith "Two conflicting --timeout-seconds outputs; you can only specify --timeout-seconds once"
                | None -> go trx filter logging par (Some (TimeSpan.FromSeconds (Int32.Parse timeoutStr |> float))) rest
            | k :: _rest -> failwith $"Unrecognised arg %s{k}"

        go None None None None None args

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
