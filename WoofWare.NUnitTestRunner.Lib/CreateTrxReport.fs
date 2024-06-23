namespace WoofWare.NUnitTestRunner

open System
open System.Reflection

/// Methods for constructing TRX reports.
[<RequireQualifiedAccess>]
module BuildTrxReport =

    /// Build a TRX report from the given results.
    let build
        (assy : Assembly)
        (creationTime : DateTimeOffset)
        (startTime : DateTimeOffset)
        (results : FixtureRunResults list)
        : TrxReport
        =
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
