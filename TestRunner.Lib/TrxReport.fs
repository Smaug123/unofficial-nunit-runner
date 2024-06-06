namespace TestRunner

open System
open System.Diagnostics
open System.IO

/// Describes the times at which a complete test run went through state transitions.
type TrxReportTimes =
    {
        Creation : DateTimeOffset
        Queuing : DateTimeOffset
        Start : DateTimeOffset
        Finish : DateTimeOffset
    }

type TrxDeployment =
    {
        RunDeploymentRoot : string
    }

type TrxTestSettings =
    {
        Name : string
        Id : Guid
        Deployment : TrxDeployment
    }

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
        StackTrace : StackTrace option
    }

/// Information output by something that runs (e.g. the test harness itself, or an individual test).
type TrxOutput =
    {
        /// What the entity printed to standard output.
        StdOut : string option
        /// Description of any error the entity encountered.
        ErrorInfo : TrxErrorInfo option
    }

type TrxUnitTestResult =
    {
        ExecutionId : Guid
        TestId : Guid
        /// This is XML-escaped for serialisation
        TestName : string
        ComputerName : string
        Duration : TimeSpan
        StartTime : DateTimeOffset
        EndTime : DateTimeOffset
        TestType : Guid
        Outcome : TrxTestOutcome
        TestListId : Guid
        RelativeResultsDirectory : Guid
        Output : TrxOutput option
    }

type TrxTestMethod =
    {
        CodeBase : FileInfo
        AdapterTypeName : Uri
        ClassName : string
        Name : string
    }

type TrxUnitTest =
    {
        Name : string
        Storage : FileInfo
        Id : Guid
        ExecutionId : Guid
        TestMethod : TrxTestMethod
    }

type TrxTestDefinitions = TrxUnitTest list

type TrxTestEntry =
    {
        TestListId : Guid
        ExecutionId : Guid
        TestId : Guid
    }

type TrxTestEntries = TrxTestEntry list

type TrxTestList =
    {
        Name : string
        Id : Guid
    }

type TrxTestLists = TrxTestList list

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

type TrxReport =
    {
        Id : Guid
        Name : string
        Times : TrxReportTimes
        Settings : TrxTestSettings
        Results : TrxUnitTestResult list
        TestDefinitions : TrxTestDefinitions
        TestEntries : TrxTestEntries
        TestLists : TrxTestLists
        ResultsSummary : TrxResultsSummary
    }

