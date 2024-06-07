namespace TestRunner

open System
open System.IO
open System.Xml

/// Describes the times at which a complete test run went through state transitions.
/// These all have semantics specific to the test runner, and I have not rigorously worked out what
/// semantics NUnit has, so take these with considerable amounts of salt.
type TrxReportTimes =
    {
        /// The time at which this test run was instantiated. (Note that this is *not* what NUnit does.
        /// I don't know what NUnit does, but it's not this.)
        Creation : DateTimeOffset
        /// Don't know what this means! But it's emitted in `dotnet test --logger trx`.
        Queuing : DateTimeOffset
        /// The time at which the first test in this run began. (Note that this is *not* what NUnit does.
        /// I don't know what NUnit does, but it's not this.)
        Start : DateTimeOffset
        /// The time at which this test run finished.
        Finish : DateTimeOffset
    }

    static member internal ofXml (node : XmlNode) : Result<TrxReportTimes, string> =
        if node.HasChildNodes then
            Error "expected <Times/> to have no child nodes"
        else

        let creation =
            match node.Attributes.["creation"] with
            | NoChildrenNode v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"could not parse '%s{v}' as a creation time"
                | true, t -> Ok t
            | _ -> Error "got no creation node"
        let queuing =
            match node.Attributes.["queuing"] with
            | NoChildrenNode v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"could not parse '%s{v}' as a queuing time"
                | true, t -> Ok t
            | _ -> Error "got no queuing node"
        let start =
            match node.Attributes.["start"] with
            | NoChildrenNode v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"could not parse '%s{v}' as a start time"
                | true, t -> Ok t
            | _ -> Error "got no start node"
        let finish =
            match node.Attributes.["finish"] with
            | NoChildrenNode v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"could not parse '%s{v}' as a finish time"
                | true, t -> Ok t
            | _ -> Error "got no finish node"

        match creation, queuing, start, finish with
        | Ok creation, Ok queuing, Ok start, Ok finish ->
            {
                Creation = creation
                Queuing = queuing
                Start = start
                Finish = finish
            }
            |> Ok
        | _ ->
            [ creation ; queuing ; start ; finish ] |> Seq.choose (function | Ok _ -> None | Error e -> Some e) |> String.concat ";"
            |> Error

/// Information about this particular instance of the test runner in space and time.
type TrxDeployment =
    {
        /// Identifier saying where and when this instance of the runner took place.
        /// Nothing else cares about this value; it's for the human's benefit.
        RunDeploymentRoot : string
    }

    static member internal ofXml (node : XmlNode) : Result<TrxDeployment, string> =
        if node.HasChildNodes then
            Error "Expected <Deployment/> node to have no child nodes"
        else

        match node.Attributes.["runDeploymentRoot"] with
        | NoChildrenNode v ->
            {
                RunDeploymentRoot = v
            }
            |> Ok
        | _ -> Error "got no runDeploymentRoot node"

/// Don't really know what this is; I'm guessing it identifies a configuration of one of the various test runners
/// that may have taken part in this test run?
type TrxTestSettings =
    {
        /// Name of one particular configuration of a test runner, human-readable.
        Name : string
        /// Machine-readable ID of this particular configuration of a test runner.
        Id : Guid
        /// Data about how this particular instance of the test runner was deployed.
        Deployment : TrxDeployment
    }

    static member internal ofXml (node : XmlNode) : Result<TrxTestSettings, string> =
        let name =
            match node.Attributes.["name"] with
            | NoChildrenNode v -> Ok v
            | _ -> Error "expected a name field on TestSettings"
        let guid =
            match node.Attributes.["id"] with
            | NoChildrenNode v ->
                match Guid.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error "could not parse id field as a GUID"
            | _ -> Error "expected an id field on TestSettings"
        let deployment =
            match node with
            | OneChildNode "Deployment" v ->
                TrxDeployment.ofXml v
            | _ -> Error "expected a <Deployment> child of TestSettings"

        match name, guid, deployment with
        | Ok name, Ok guid, Ok deployment ->
            {
                Name = name
                Id = guid
                Deployment = deployment
            }
            |> Ok
        | _ ->
            [ Result.getError name ; Result.getError guid ; Result.getError deployment ] |> Seq.choose id |> String.concat ";"
            |> Error

/// The outcome of a specific single test.
type TrxTestOutcome =
    /// The test passed.
    | Passed
    /// The test failed.
    | Failed

    /// Serialisation suitable for direct interpolation into a TRX report.
    override this.ToString () =
        match this with
        | TrxTestOutcome.Passed -> "Passed"
        | TrxTestOutcome.Failed -> "Failed"

/// Description of an error emitted by something that runs (e.g. the test harness or an individual test).
/// This is not a description of a test *failure*.
type TrxErrorInfo =
    {
        /// Description of the error.
        Message : string
        /// Stack trace if this error arose from an exception.
        StackTrace : string option
    }

    static member internal ofXml (node : XmlNode) : Result<TrxErrorInfo, string> =
        let message =
            match node with
            | NodeWithNamedChild "Message" (NoChildrenNode message) -> Some message
            | _ -> None
        let stackTrace =
            match node with
            | NodeWithNamedChild "StackTrace" (NoChildrenNode stackTrace) -> Some stackTrace
            | _ -> None

        match message with
        | None -> Error "Expected a Message node"
        | Some message ->
            {
                Message = message
                StackTrace = stackTrace
            }
            |> Ok

/// Information output by something that runs (e.g. the test harness itself, or an individual test).
type TrxOutput =
    {
        /// What the entity printed to standard output.
        StdOut : string option
        /// Description of any error the entity encountered.
        ErrorInfo : TrxErrorInfo option
    }

    static member internal ofXml (node : XmlNode) : Result<TrxOutput, string> =
        let stdout =
            match node with
            | NodeWithNamedChild "StdOut" (NoChildrenNode stdout) -> Some stdout
            | _ -> None

        let errorInfo =
            match node with
            | NodeWithNamedChild "ErrorInfo" node ->
                match TrxErrorInfo.ofXml node with
                | Ok n -> Ok (Some n)
                | Error e -> Error e
            | _ -> Ok None

        match errorInfo with
        | Error e -> Error e
        | Ok errorInfo ->
            {
                StdOut = stdout
                ErrorInfo = errorInfo
            }
            |> Ok

type TrxUnitTestResult =
    {
        /// An ID that lets us identify this test result among the executions (see the <UnitTest> data object).
        ExecutionId : Guid
        /// An ID that lets us identify this test result among the tests (see the <UnitTest> data object).
        TestId : Guid
        /// Name of this test, likely the same as the name of the method that defined it.
        TestName : string
        /// Name of the computer on which this test ran.
        ComputerName : string
        /// How long this test took to run.
        Duration : TimeSpan
        /// When this test started.
        StartTime : DateTimeOffset
        /// When this test finished.
        EndTime : DateTimeOffset
        /// An undocumented GUID that is always 13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b and appears to mean "automated
        /// test".
        TestType : Guid
        /// Outcome of this test.
        Outcome : TrxTestOutcome
        /// An ID that lets us identify what collection of tests this was part of (see the <TestLists> data object).
        TestListId : Guid
        /// It's awfully unclear what this is, but it seems to take the same value as ExecutionId.
        RelativeResultsDirectory : string
        /// Any output the test had, beyond its passed/failed (etc) outcome.
        Output : TrxOutput option
    }
    static member internal ofXml (node : XmlNode) : Result<TrxUnitTestResult, string> =
        failwith ""

type TrxTestMethod =
    {
        CodeBase : FileInfo
        AdapterTypeName : Uri
        ClassName : string
        Name : string
    }
    static member internal ofXml (node : XmlNode) : Result<TrxTestMethod, string> =
        failwith ""

type TrxUnitTest =
    {
        Name : string
        Storage : FileInfo
        Id : Guid
        ExecutionId : Guid
        TestMethod : TrxTestMethod
    }
    static member internal ofXml (node : XmlNode) : Result<TrxUnitTest, string> =
        failwith ""

type TrxTestEntry =
    {
        TestListId : Guid
        ExecutionId : Guid
        TestId : Guid
    }
    static member internal ofXml (node : XmlNode) : Result<TrxTestEntry, string> =
        failwith ""

type TrxTestList =
    {
        Name : string
        Id : Guid
    }
    static member internal ofXml (node : XmlNode) : Result<TrxTestList, string> =
        failwith ""

type TrxOutcome =
    | Completed
    | Warning
    | Failed

    /// Serialisation suitable for direct interpolation into a TRX report.
    override this.ToString () =
        match this with
        | TrxOutcome.Warning -> "Warning"
        | TrxOutcome.Completed -> "Completed"
        | TrxOutcome.Failed -> "Failed"

/// Ancillary information about text emitted during a complete test run.
type TrxRunInfo =
    {
        /// The computer on which this text was emitted.
        ComputerName : string
        Outcome : TrxOutcome
        /// When this text was emitted.
        Timestamp : DateTimeOffset
        /// The text which was emitted in this event within the run.
        Text : string
    }
    static member internal ofXml (node : XmlNode) : Result<TrxRunInfo, string> =
        failwith ""

type TrxCounters =
    {
        Total : uint
        Executed : uint
        Passed : uint
        Failed : uint
        Errors : uint
        Timeout : uint
        Aborted : uint
        Inconclusive : uint
        PassedButRunAborted : uint
        NotRunnable : uint
        NotExecuted : uint
        Disconnected : uint
        Warning : uint
        Completed : uint
        InProgress : uint
        Pending : uint
    }
    static member internal ofXml (node : XmlNode) : Result<TrxCounters, string> =
        failwith ""

    /// A set of counters where every counter is 0.
    static member Zero =
        {
            Total = 0u
            Executed = 0u
            Passed = 0u
            Failed = 0u
            Errors = 0u
            Timeout = 0u
            Aborted = 0u
            Inconclusive = 0u
            PassedButRunAborted = 0u
            NotRunnable = 0u
            NotExecuted = 0u
            Disconnected = 0u
            Warning = 0u
            Completed = 0u
            InProgress = 0u
            Pending = 0u
        }

type TrxResultsSummary =
    {
        Outcome : TrxOutcome
        Counters : TrxCounters
        Output : TrxOutput
        RunInfos : TrxRunInfo list
    }
    static member internal ofXml (node : XmlNode) : Result<TrxResultsSummary, string> =
        failwith ""

/// A report on a test run.
type TrxReport =
    {
        Id : Guid
        Name : string
        Times : TrxReportTimes
        Settings : TrxTestSettings
        Results : TrxUnitTestResult list
        TestDefinitions : TrxUnitTest list
        TestEntries : TrxTestEntry list
        TestLists : TrxTestList list
        ResultsSummary : TrxResultsSummary
    }
    static member internal ofXml (node : XmlNode) : Result<TrxReport, string> =
        failwith ""

/// A report on a test run.
[<RequireQualifiedAccess>]
module TrxReport =
    /// Parse the report. May instead return a human-readable description of why we couldn't parse.
    let parse (s : string) : Result<TrxReport, string> =
        let node = XmlDocument ()
        node.LoadXml s
        TrxReport.ofXml node
