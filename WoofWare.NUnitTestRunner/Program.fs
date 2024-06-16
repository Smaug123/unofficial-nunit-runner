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
        | [] -> failwith "Supply a positional arg, the DLL to test."
        | dll :: rest ->

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
                | Some f -> failwith "Two conflicting filters; you can only specify --filter once"
                | None -> go trx (Some (Filter.parse filterStr)) logging par timeout rest
            | Key "--trx" trxStr :: rest
            | "--trx" :: trxStr :: rest ->
                match trx with
                | Some f -> failwith "Two conflicting TRX outputs; you can only specify --trx once"
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

        let levelOfParallelism, par =
            ((None, None), assy.CustomAttributes)
            ||> Seq.fold (fun (levelPar, par) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.LevelOfParallelismAttribute" ->
                    let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<int>

                    match levelPar with
                    | None -> (Some arg, par)
                    | Some existing ->
                        failwith $"Assembly %s{assy.Location} declares parallelism %i{arg} and also %i{existing}"
                | "NUnit.Framework.NonParallelizableAttribute" ->
                    match levelPar with
                    | None -> (Some 1, par)
                    | Some existing ->
                        failwith
                            $"Assembly %s{assy.Location} declares non-parallelizable and also parallelism %i{existing}"
                | "NUnit.Framework.ParallelizableAttribute" ->
                    match par with
                    | Some _ -> failwith "Got multiple Parallelize attributes in assembly"
                    | None ->
                        match attr.ConstructorArguments |> Seq.toList with
                        | [] -> levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Fixtures)
                        | [ v ] ->
                            match v.Value with
                            | :? int as v ->
                                match v with
                                | 512 -> levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Fixtures)
                                | 256 -> levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Children)
                                | 257 ->
                                    failwith "ParallelScope.All is invalid on assemblies; only Fixtures or Children"
                                | 1 ->
                                    failwith "ParallelScope.Self is invalid on assemblies; only Fixtures or Children"
                                | v -> failwith $"Could not recognise value %i{v} of parallel scope on assembly"
                            | v -> failwith $"Unexpectedly non-int value %O{v} of parallel scope on assembly"
                        | _ -> failwith "unexpectedly got multiple args to Parallelizable on assembly"
                | _ -> levelPar, par
            )

        let levelOfParallelism =
            match args.LevelOfParallelism, levelOfParallelism with
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

        use par = new ParallelQueue (levelOfParallelism, par)

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

        let finishTime = DateTimeOffset.Now
        let finishTimeHumanReadable = finishTime.ToString @"yyyy-MM-dd HH:mm:ss"
        let nowMachine = finishTime.ToString @"yyyy-MM-dd_HH_mm_ss"

        let testListId = Guid.NewGuid ()

        let testDefinitions, testEntries =
            results
            |> List.collect (fun results -> results.IndividualTestRunMetadata)
            |> List.map (fun (data, _) ->
                let defn =
                    {
                        Name = data.TestName
                        Storage = assy.Location.ToLowerInvariant ()
                        Id = data.TestId
                        Execution =
                            {
                                Id = data.ExecutionId
                            }
                        TestMethod =
                            {
                                CodeBase = assy.Location
                                AdapterTypeName = Uri "executor://woofware/"
                                ClassName = data.ClassName
                                Name = data.TestName
                            }
                    }

                let entry : TrxTestEntry =
                    {
                        TestListId = testListId
                        ExecutionId = data.ExecutionId
                        TestId = data.TestId

                    }

                defn, entry
            )
            |> List.unzip

        let hostname = Environment.MachineName

        let settings =
            {
                Name = "default"
                Id = Guid.NewGuid ()
                Deployment =
                    {
                        RunDeploymentRoot = $"_%s{hostname}_%s{nowMachine}"
                    }
            }

        let testList : TrxTestListEntry =
            {
                Id = testListId
                Name = "All"
            }

        let counters =
            (TrxCounters.Zero, results)
            // TODO: this is woefully inefficient
            ||> List.fold (fun counters results ->
                let counters =
                    (counters, results.Failed)
                    ||> List.fold (fun counters (_, _) ->
                        // TODO: the counters can be more specific about the failure mode
                        counters.AddFailed ()
                    )

                let counters =
                    (counters, results.OtherFailures)
                    ||> List.fold (fun counters _ ->
                        // TODO: the counters can be more specific about the failure mode
                        counters.AddFailed ()
                    )

                (counters, results.Success)
                ||> List.fold (fun counters (_, success, _) ->
                    match success with
                    | TestMemberSuccess.Ok -> counters.AddPassed ()
                    | TestMemberSuccess.Ignored _
                    | TestMemberSuccess.Explicit _ -> counters.AddNotExecuted ()
                    | TestMemberSuccess.Inconclusive _ -> counters.AddInconclusive ()
                )
            )

        // TODO: I'm sure we can do better than this; there's a whole range of possible
        // states!
        let outcome =
            if counters.Failed > 0u then
                TrxOutcome.Failed
            else
                TrxOutcome.Completed

        let resultSummary : TrxResultsSummary =
            {
                Outcome = outcome
                Counters = counters
                Output =
                    {
                        StdOut = None
                        StdErr = None
                        ErrorInfo = None
                    }
                RunInfos =
                    [
                    // TODO: capture stdout
                    ]
            }

        let times : TrxReportTimes =
            {
                Creation = creationTime
                Queuing = startTime
                Start = startTime
                Finish = finishTime

            }

        let magicGuid = Guid.Parse "13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b"

        let results =
            results
            |> List.collect (fun results -> results.IndividualTestRunMetadata)
            |> List.map (fun (i, cause) ->
                let exc =
                    match cause with
                    | Choice2Of3 _ -> None
                    | Choice1Of3 (TestMemberFailure.Malformed reasons) ->
                        {
                            StackTrace = None
                            Message = reasons |> String.concat "\n" |> Some
                        }
                        |> Some
                    | Choice1Of3 (TestMemberFailure.Failed fail)
                    | Choice1Of3 (TestMemberFailure.Failed fail)
                    | Choice1Of3 (TestMemberFailure.Failed fail) ->
                        ((None, None), fail)
                        ||> List.fold (fun (stackTrace, message) tf ->
                            match tf with
                            | TestFailure.TestFailed (UserMethodFailure.Threw (_, exc))
                            | TestFailure.SetUpFailed (UserMethodFailure.Threw (_, exc))
                            | TestFailure.TearDownFailed (UserMethodFailure.Threw (_, exc)) ->
                                let stackTrace =
                                    match stackTrace with
                                    | None -> (exc : Exception).ToString ()
                                    | Some s -> s

                                (Some stackTrace, message)
                            | TestFailure.TestFailed (UserMethodFailure.ReturnedNonUnit (_, ret))
                            | TestFailure.SetUpFailed (UserMethodFailure.ReturnedNonUnit (_, ret))
                            | TestFailure.TearDownFailed (UserMethodFailure.ReturnedNonUnit (_, ret)) ->
                                let newMessage = $"returned non-unit value %O{ret}"

                                let message =
                                    match message with
                                    | None -> newMessage
                                    | Some message -> $"%s{message}\n%s{newMessage}"

                                (stackTrace, Some message)
                        )
                        |> fun (stackTrace, message) ->
                            {
                                StackTrace = stackTrace
                                Message = message
                            }
                            |> Some
                    | Choice3Of3 (UserMethodFailure.Threw (_, exc)) ->
                        {
                            StackTrace = (exc : Exception).ToString () |> Some
                            Message = None
                        }
                        |> Some
                    | Choice3Of3 (UserMethodFailure.ReturnedNonUnit (_, ret)) ->
                        {
                            Message = $"returned non-unit value %O{ret}" |> Some
                            StackTrace = None
                        }
                        |> Some

                let outcome =
                    match cause with
                    | Choice1Of3 _ -> TrxTestOutcome.Failed
                    | Choice2Of3 TestMemberSuccess.Ok -> TrxTestOutcome.Passed
                    | Choice2Of3 (TestMemberSuccess.Inconclusive _) -> TrxTestOutcome.Inconclusive
                    | Choice2Of3 (TestMemberSuccess.Ignored _)
                    | Choice2Of3 (TestMemberSuccess.Explicit _) -> TrxTestOutcome.NotExecuted
                    // TODO: we can totally do better here, more fine-grained classification
                    | Choice3Of3 _ -> TrxTestOutcome.Failed

                {
                    ExecutionId = i.ExecutionId
                    TestId = i.TestId
                    TestName = i.TestName
                    ComputerName = i.ComputerName
                    Duration = i.End - i.Start
                    StartTime = i.Start
                    EndTime = i.End
                    TestType = magicGuid
                    Outcome = outcome
                    TestListId = testListId
                    RelativeResultsDirectory = i.ExecutionId.ToString () // for some reason
                    Output =
                        match i.StdOut, i.StdErr, exc with
                        | None, None, None -> None
                        | stdout, stderr, exc ->
                            Some
                                {
                                    TrxOutput.StdOut = stdout
                                    StdErr = stderr
                                    ErrorInfo = exc
                                }
                }
            )

        let report : TrxReport =
            {
                Id = Guid.NewGuid ()
                Name = $"@%s{hostname} %s{finishTimeHumanReadable}"
                Times = times
                Settings = settings
                Results = results
                TestDefinitions = testDefinitions
                TestEntries = testEntries
                TestLists = [ testList ]
                ResultsSummary = resultSummary
            }

        match args.Trx with
        | Some trxPath ->
            let contents = TrxReport.toXml report |> fun d -> d.OuterXml
            trxPath.Directory.Create ()
            File.WriteAllText (trxPath.FullName, contents)
            Console.Error.WriteLine $"Written TRX file: %s{trxPath.FullName}"
        | None -> ()

        match outcome with
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
