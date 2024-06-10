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

        let desiredVersions =
            match config.Framework with
            | Some f -> [ Version f.Version, f.Name ]
            | None ->

            match config.Frameworks with
            | Some f -> f |> List.map (fun f -> Version f.Version, f.Name)
            | None ->
                failwith
                    "Could not deduce a framework version due to lack of either Framework or Frameworks in runtimeconfig"

        let compatiblyNamedRuntimes =
            f.Frameworks
            |> Seq.collect (fun availableFramework ->
                desiredVersions
                |> List.choose (fun (desiredVersion, desiredName) ->
                    if desiredName = availableFramework.Name then
                        Some
                            {|
                                Desired = desiredVersion
                                Name = desiredName
                                Installed = availableFramework
                                InstalledVersion = Version availableFramework.Version
                            |}
                    else
                        None
                )
            )
            |> Seq.toList

        match rollForward with
        | RollForward.Minor ->
            let available =
                compatiblyNamedRuntimes
                |> Seq.filter (fun data ->
                    data.InstalledVersion.Major = data.Desired.Major
                    && data.InstalledVersion.Minor >= data.Desired.Minor
                )
                |> Seq.groupBy (fun data -> data.Name)
                |> Seq.map (fun (name, data) ->
                    let data =
                        data
                        |> Seq.minBy (fun data -> data.InstalledVersion.Minor, data.InstalledVersion.Build)

                    name, data.Installed
                )
                // TODO: how do we select between many available frameworks?
                |> Seq.tryHead

            match available with
            | Some (_, f) -> Some (Choice1Of2 f)
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
        | Some (Choice1Of2 runtime) -> [ dll.Directory ; DirectoryInfo $"%s{runtime.Path}/%s{runtime.Version}" ]
        | Some (Choice2Of2 sdk) -> [ dll.Directory ; DirectoryInfo sdk.Path ]

    let main argv =
        let startTime = DateTimeOffset.Now

        let testDll, filter, trxPath =
            match argv |> List.ofSeq with
            | [ dll ] -> FileInfo dll, None, None
            | [ dll ; "--trx" ; trxPath ] -> FileInfo dll, None, Some (FileInfo trxPath)
            | [ dll ; "--filter" ; filter ] -> FileInfo dll, Some (Filter.parse filter), None
            | [ dll ; "--trx" ; trxPath ; "--filter" ; filter ] ->
                FileInfo dll, Some (Filter.parse filter), Some (FileInfo trxPath)
            | [ dll ; "--filter" ; filter ; "--trx" ; trxPath ] ->
                FileInfo dll, Some (Filter.parse filter), Some (FileInfo trxPath)
            | _ ->
                failwith
                    "provide exactly one arg, a test DLL; then optionally `--filter <filter>` and/or `--trx <output-filename>`."

        let filter =
            match filter with
            | Some filter -> Filter.shouldRun filter
            | None -> fun _ _ -> true

        let progress = Progress.spectre ()

        use _ = new SetBaseDir (testDll)

        let ctx = Ctx (testDll, locateRuntimes testDll)
        let assy = ctx.LoadFromAssemblyPath testDll.FullName

        let testFixtures = assy.ExportedTypes |> Seq.map TestFixture.parse |> Seq.toList

        let creationTime = DateTimeOffset.Now
        let results = testFixtures |> List.map (TestFixture.run progress filter)

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
                        // TODO surely stderr can be emitted
                        | stdout, _stderr, exc ->
                            Some
                                {
                                    TrxOutput.StdOut = stdout
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

        match trxPath with
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
