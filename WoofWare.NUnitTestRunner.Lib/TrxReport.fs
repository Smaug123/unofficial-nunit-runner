namespace WoofWare.NUnitTestRunner

open System
open System.Xml

[<RequireQualifiedAccess>]
module private XmlUtil =
    [<Literal>]
    let NS = "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"

/// Describes the times at which a complete test run went through state transitions.
/// These all have semantics specific to the test runner, and I have not rigorously worked out what
/// semantics NUnit has, so take these with considerable amounts of salt.
[<NoComparison>]
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

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("Times", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "creation"
            attr.Value <- this.Creation.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "queuing"
            attr.Value <- this.Queuing.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "start"
            attr.Value <- this.Start.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "finish"
            attr.Value <- this.Finish.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxReportTimes, string> =
        if node.HasChildNodes then
            Error "expected <Times/> to have no child nodes"
        else

        let creation =
            match node.Attributes.["creation"] with
            | null -> Error "got no creation node"
            | v ->
                match DateTimeOffset.TryParse v.Value with
                | false, _ -> Error $"could not parse '%s{v.Value}' as a creation time"
                | true, t -> Ok t

        let queuing =
            match node.Attributes.["queuing"] with
            | null -> Error "got no queuing node"
            | v ->
                match DateTimeOffset.TryParse v.Value with
                | false, _ -> Error $"could not parse '%s{v.Value}' as a queuing time"
                | true, t -> Ok t

        let start =
            match node.Attributes.["start"] with
            | null -> Error "got no start node"
            | v ->
                match DateTimeOffset.TryParse v.Value with
                | false, _ -> Error $"could not parse '%s{v.Value}' as a start time"
                | true, t -> Ok t

        let finish =
            match node.Attributes.["finish"] with
            | null -> Error "got no finish node"
            | v ->
                match DateTimeOffset.TryParse v.Value with
                | false, _ -> Error $"could not parse '%s{v.Value}' as a finish time"
                | true, t -> Ok t

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
            [ creation ; queuing ; start ; finish ]
            |> Seq.choose (
                function
                | Ok _ -> None
                | Error e -> Some e
            )
            |> String.concat "; "
            |> Error

/// Information about this particular instance of the test runner in space and time.
[<NoComparison>]
type TrxDeployment =
    {
        /// Identifier saying where and when this instance of the runner took place.
        /// Nothing else cares about this value; it's for the human's benefit.
        RunDeploymentRoot : string
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("Deployment", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "runDeploymentRoot"
            attr.Value <- this.RunDeploymentRoot
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxDeployment, string> =
        if node.HasChildNodes then
            Error "Expected <Deployment/> node to have no child nodes"
        else

        match node.Attributes.["runDeploymentRoot"] with
        | null -> Error "got no runDeploymentRoot node"
        | v ->
            {
                RunDeploymentRoot = v.Value
            }
            |> Ok

/// Don't really know what this is; I'm guessing it identifies a configuration of one of the various test runners
/// that may have taken part in this test run?
[<NoComparison>]
type TrxTestSettings =
    {
        /// Name of one particular configuration of a test runner, human-readable.
        Name : string
        /// Machine-readable ID of this particular configuration of a test runner.
        Id : Guid
        /// Data about how this particular instance of the test runner was deployed.
        Deployment : TrxDeployment
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("TestSettings", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "name"
            attr.Value <- this.Name
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "id"
            attr.Value <- this.Id.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do node.AppendChild (this.Deployment.toXml doc) |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxTestSettings, string> =
        let name =
            match node.Attributes.["name"] with
            | null -> Error "expected a name field on TestSettings"
            | v -> Ok v.Value

        let guid =
            match node.Attributes.["id"] with
            | null -> Error "expected an id field on TestSettings"
            | v ->
                match Guid.TryParse v.Value with
                | true, v -> Ok v
                | false, _ -> Error "could not parse id field as a GUID"

        let deployment =
            match node with
            | NodeWithNamedChild "Deployment" v -> TrxDeployment.ofXml v
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
            [ Result.getError name ; Result.getError guid ; Result.getError deployment ]
            |> Seq.choose id
            |> String.concat "; "
            |> Error

/// The outcome of a specific single test.
[<NoComparison>]
type TrxTestOutcome =
    /// The test passed.
    | Passed
    /// The test failed.
    | Failed
    /// The test was not executed.
    | NotExecuted
    /// The test was inconclusive. (This appears not to be modelled correctly by NUnit! They use NotExecuted.)
    | Inconclusive

    /// Serialisation suitable for direct interpolation into a TRX report.
    override this.ToString () =
        match this with
        | TrxTestOutcome.Passed -> "Passed"
        | TrxTestOutcome.Failed -> "Failed"
        | TrxTestOutcome.NotExecuted -> "NotExecuted"
        | TrxTestOutcome.Inconclusive -> "Inconclusive"

    /// Round-trips with `ToString`; returns None if parse was unsuccessful.
    static member Parse (s : string) : TrxTestOutcome option =
        if s.Equals ("passed", StringComparison.OrdinalIgnoreCase) then
            Some TrxTestOutcome.Passed
        elif s.Equals ("failed", StringComparison.OrdinalIgnoreCase) then
            Some TrxTestOutcome.Failed
        elif s.Equals ("notexecuted", StringComparison.OrdinalIgnoreCase) then
            Some TrxTestOutcome.NotExecuted
        elif s.Equals ("inconclusive", StringComparison.OrdinalIgnoreCase) then
            Some TrxTestOutcome.Inconclusive
        else
            None

/// Description of an error emitted by something that runs (e.g. the test harness or an individual test).
/// This is not a description of a test *failure*.
[<NoComparison>]
type TrxErrorInfo =
    {
        /// Description of the error.
        Message : string option
        /// Stack trace if this error arose from an exception.
        StackTrace : string option
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("ErrorInfo", XmlUtil.NS)

        match this.Message with
        | None -> ()
        | Some message ->
            let child = doc.CreateTextNode message
            let messageNode = doc.CreateElement ("Message", XmlUtil.NS)
            messageNode.AppendChild child |> ignore<XmlNode>
            node.AppendChild messageNode |> ignore<XmlNode>

        match this.StackTrace with
        | None -> ()
        | Some stackTrace ->
            let child = doc.CreateTextNode stackTrace
            let stackTraceNode = doc.CreateElement ("StackTrace", XmlUtil.NS)
            stackTraceNode.AppendChild child |> ignore<XmlNode>
            node.AppendChild stackTraceNode |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxErrorInfo, string> =
        let message =
            match node with
            | NodeWithNamedChild "Message" (OneChildNode "Message" (NoChildrenNode message)) -> Some message
            | _ -> None

        let stackTrace =
            match node with
            | NodeWithNamedChild "StackTrace" (OneChildNode "StackTrace" (NoChildrenNode stackTrace)) -> Some stackTrace
            | _ -> None

        {
            Message = message
            StackTrace = stackTrace
        }
        |> Ok

/// Information output by something that runs (e.g. the test harness itself, or an individual test).
[<NoComparison>]
type TrxOutput =
    {
        /// What the entity printed to standard output.
        StdOut : string option
        /// Description of any error the entity encountered.
        ErrorInfo : TrxErrorInfo option
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("Output", XmlUtil.NS)

        match this.StdOut with
        | None -> ()
        | Some stdout ->
            let text = doc.CreateTextNode stdout
            let childNode = doc.CreateElement ("StdOut", XmlUtil.NS)
            childNode.AppendChild text |> ignore<XmlNode>
            node.AppendChild childNode |> ignore<XmlNode>

        match this.ErrorInfo with
        | None -> ()
        | Some errInfo -> node.AppendChild (errInfo.toXml doc) |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxOutput, string> =
        let stdout =
            match node with
            | NodeWithNamedChild "StdOut" (OneChildNode "StdOut" (NoChildrenNode stdout)) -> Some stdout
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

/// The result of a run of a single test.
[<NoComparison>]
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

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("UnitTestResult", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "executionId"
            attr.Value <- this.ExecutionId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "testId"
            attr.Value <- this.TestId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "testName"
            attr.Value <- this.TestName
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "computerName"
            attr.Value <- this.ComputerName
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "duration"
            attr.Value <- this.Duration.ToString "c"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "startTime"
            attr.Value <- this.StartTime.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "endTime"
            attr.Value <- this.EndTime.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "testType"
            attr.Value <- this.TestType.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "outcome"
            attr.Value <- this.Outcome.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "testListId"
            attr.Value <- this.TestListId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "relativeResultsDirectory"
            attr.Value <- this.RelativeResultsDirectory
            node.Attributes.Append attr |> ignore<XmlAttribute>

        match this.Output with
        | None -> ()
        | Some output -> node.AppendChild (output.toXml doc) |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxUnitTestResult, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let executionId =
            match attrs.TryGetValue "executionId" with
            | false, _ -> Error "executionId"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"executionId was not a GUID: %s{v}"
                | true, v -> Ok v

        let testId =
            match attrs.TryGetValue "testId" with
            | false, _ -> Error "testId"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"testId was not a GUID: %s{v}"
                | true, v -> Ok v

        let testName =
            match attrs.TryGetValue "testName" with
            | false, _ -> Error "testName"
            | true, v -> Ok v

        let computerName =
            match attrs.TryGetValue "computerName" with
            | false, _ -> Error "computerName"
            | true, v -> Ok v

        let duration =
            match attrs.TryGetValue "duration" with
            | false, _ -> Error "duration"
            | true, v ->
                match TimeSpan.TryParse v with
                | false, _ -> Error $"duration was not a TimeSpan: %s{v}"
                | true, v -> Ok v

        let startTime =
            match attrs.TryGetValue "startTime" with
            | false, _ -> Error "startTime"
            | true, v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"startTime was not a DateTimeOffset: %s{v}"
                | true, v -> Ok v

        let endTime =
            match attrs.TryGetValue "endTime" with
            | false, _ -> Error "endTime"
            | true, v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"endTime was not a DateTimeOffset: %s{v}"
                | true, v -> Ok v

        let testType =
            match attrs.TryGetValue "testType" with
            | false, _ -> Error "testType"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"testType was not a GUID: %s{v}"
                | true, v -> Ok v

        let outcome =
            match attrs.TryGetValue "outcome" with
            | false, _ -> Error "outcome"
            | true, v ->
                match TrxTestOutcome.Parse v with
                | None -> Error $"Outcome was not recognised: %s{v}"
                | Some v -> Ok v

        let testListId =
            match attrs.TryGetValue "testListId" with
            | false, _ -> Error "testListId"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"testListId was not a GUID: %s{v}"
                | true, v -> Ok v

        let relativeResultsDirectory =
            match attrs.TryGetValue "relativeResultsDirectory" with
            | false, _ -> Error "relativeResultsDirectory"
            | true, v -> Ok v

        let output =
            match node with
            | NodeWithNamedChild "Output" node -> Some (TrxOutput.ofXml node)
            | _ -> None

        let errors =
            [
                executionId |> Result.getError
                testId |> Result.getError
                testName |> Result.getError
                computerName |> Result.getError
                duration |> Result.getError
                startTime |> Result.getError
                endTime |> Result.getError
                testType |> Result.getError
                outcome |> Result.getError
                testListId |> Result.getError
                relativeResultsDirectory |> Result.getError
                output |> Option.bind Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let executionId = executionId |> Result.get |> Option.get
        let testId = testId |> Result.get |> Option.get
        let testName = testName |> Result.get |> Option.get
        let computerName = computerName |> Result.get |> Option.get
        let duration = duration |> Result.get |> Option.get
        let startTime = startTime |> Result.get |> Option.get
        let endTime = endTime |> Result.get |> Option.get
        let testType = testType |> Result.get |> Option.get
        let outcome = outcome |> Result.get |> Option.get
        let testListId = testListId |> Result.get |> Option.get
        let relativeResultsDirectory = relativeResultsDirectory |> Result.get |> Option.get
        let output = output |> Option.bind Result.get

        {
            ExecutionId = executionId
            TestId = testId
            TestName = testName
            ComputerName = computerName
            Duration = duration
            StartTime = startTime
            EndTime = endTime
            TestType = testType
            Outcome = outcome
            TestListId = testListId
            RelativeResultsDirectory = relativeResultsDirectory
            Output = output
        }
        |> Ok

/// A method which, being run, caused a test to run.
/// You get one of these for each run, so in particular you get a different one for each parameter of the method.
[<NoComparison>]
type TrxTestMethod =
    {
        /// Path to the DLL from which this test was extracted.
        CodeBase : string
        /// Identifier for the entity or framework which executed this test.
        AdapterTypeName : Uri
        /// Name of the .NET class in which this test method was defined.
        ClassName : string
        /// Name of the test method. This includes string representations of any parameters to the method.
        Name : string
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("TestMethod", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "codeBase"
            attr.Value <- this.CodeBase
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "adapterTypeName"
            attr.Value <- this.AdapterTypeName.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "className"
            attr.Value <- this.ClassName
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "name"
            attr.Value <- this.Name
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxTestMethod, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let codeBase =
            match attrs.TryGetValue "codeBase" with
            | false, _ -> Error "Expected codeBase attribute"
            | true, v -> Ok v

        let adapterTypeName =
            match attrs.TryGetValue "adapterTypeName" with
            | false, _ -> Error "Expected adapterTypeName attribute"
            | true, v -> Uri v |> Ok

        let className =
            match attrs.TryGetValue "className" with
            | false, _ -> Error "Expected className attribute"
            | true, v -> Ok v

        let name =
            match attrs.TryGetValue "name" with
            | false, _ -> Error "Expected name attribute"
            | true, v -> Ok v

        let errors =
            [
                codeBase |> Result.getError
                adapterTypeName |> Result.getError
                className |> Result.getError
                name |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let codeBase = codeBase |> Result.get |> Option.get
        let adapterTypeName = adapterTypeName |> Result.get |> Option.get
        let className = className |> Result.get |> Option.get
        let name = name |> Result.get |> Option.get

        {
            CodeBase = codeBase
            AdapterTypeName = adapterTypeName
            ClassName = className
            Name = name
        }
        |> Ok

/// Information about a single "execution"; it's not entirely clear to me what this is.
[<NoComparison>]
type TrxExecution =
    {
        /// Identifier for the execution that ran this specific test.
        Id : Guid
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("Execution", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "id"
            attr.Value <- this.Id.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxExecution, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let ident =
            match attrs.TryGetValue "id" with
            | false, _ -> Error "Expected id attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let errors = [ ident |> Result.getError ] |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let ident = ident |> Result.get |> Option.get

        {
            Id = ident
        }
        |> Ok


/// A single test. (So, for example, you get different ones of these for each parameter to the test method.)
[<NoComparison>]
type TrxUnitTest =
    {
        /// Name of the test, incorporating string representations of any parameters to the test.
        Name : string
        /// Not really sure what this is, but in my runs it's always been a path to the test DLL.
        Storage : string
        /// Identifier for this test, for cross-referencing in the TestEntries and Results lists.
        Id : Guid
        /// Identifier for the execution that ran this specific test.
        Execution : TrxExecution
        /// The method we ran to run this test.
        TestMethod : TrxTestMethod
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("UnitTest", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "name"
            attr.Value <- this.Name
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "storage"
            attr.Value <- this.Storage
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "id"
            attr.Value <- this.Id.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node.AppendChild (this.Execution.toXml doc) |> ignore<XmlNode>
        node.AppendChild (this.TestMethod.toXml doc) |> ignore<XmlNode>
        node

    static member internal ofXml (node : XmlNode) : Result<TrxUnitTest, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let name =
            match attrs.TryGetValue "name" with
            | false, _ -> Error "Expected name attribute"
            | true, v -> Ok v

        let storage =
            match attrs.TryGetValue "storage" with
            | false, _ -> Error "Expected storage attribute"
            | true, v -> Ok v

        let ident =
            match attrs.TryGetValue "id" with
            | false, _ -> Error "Expected id attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let execution =
            match node with
            | NodeWithNamedChild "Execution" node -> TrxExecution.ofXml node
            | _ -> Error "Expected Execution node, but got none"

        let testMethod =
            match node with
            | NodeWithNamedChild "TestMethod" node -> TrxTestMethod.ofXml node
            | _ -> Error "Expected TestMethod node, but got none"

        let errors =
            [
                name |> Result.getError
                storage |> Result.getError
                ident |> Result.getError
                execution |> Result.getError
                testMethod |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let name = name |> Result.get |> Option.get
        let storage = storage |> Result.get |> Option.get
        let ident = ident |> Result.get |> Option.get
        let execution = execution |> Result.get |> Option.get
        let testMethod = testMethod |> Result.get |> Option.get

        {
            Name = name
            Storage = storage
            Id = ident
            Execution = execution
            TestMethod = testMethod
        }
        |> Ok

/// Essentially a mapping that tells you which list and execution a test is in.
[<NoComparison>]
type TrxTestEntry =
    {
        /// For cross-referencing with the TestLists list.
        TestListId : Guid
        /// Don't really know what this means.
        ExecutionId : Guid
        /// For cross-referencing with the TestDefinitions list.
        TestId : Guid
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("TestEntry", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "testListId"
            attr.Value <- this.TestListId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "executionId"
            attr.Value <- this.ExecutionId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "testId"
            attr.Value <- this.TestId.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxTestEntry, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let testListId =
            match attrs.TryGetValue "testListId" with
            | false, _ -> Error "Expected name testListId"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let testId =
            match attrs.TryGetValue "testId" with
            | false, _ -> Error "Expected testId attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let executionId =
            match attrs.TryGetValue "executionId" with
            | false, _ -> Error "Expected executionId attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let errors =
            [
                testListId |> Result.getError
                testId |> Result.getError
                executionId |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let testListId = testListId |> Result.get |> Option.get
        let testId = testId |> Result.get |> Option.get
        let executionId = executionId |> Result.get |> Option.get

        {
            TestListId = testListId
            TestId = testId
            ExecutionId = executionId
        }
        |> Ok

/// An entry in the TestList, assigning a name to each TestListId.
[<NoComparison>]
type TrxTestListEntry =
    {
        /// Human-readable name of this list.
        Name : string
        /// ID of this list, for cross-referencing with other tests' TestListId.
        Id : Guid
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("TestList", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "name"
            attr.Value <- this.Name
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "id"
            attr.Value <- this.Id.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxTestListEntry, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let name =
            match attrs.TryGetValue "name" with
            | false, _ -> Error "Expected name attribute"
            | true, v -> Ok v

        let ident =
            match attrs.TryGetValue "id" with
            | false, _ -> Error "Expected id attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let errors =
            [ name |> Result.getError ; ident |> Result.getError ] |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let name = name |> Result.get |> Option.get
        let ident = ident |> Result.get |> Option.get

        {
            Name = name
            Id = ident
        }
        |> Ok

/// Outcome of a test run or of a test.
[<NoComparison>]
type TrxOutcome =
    /// Test run was successful.
    | Completed
    /// Dunno why this can be emitted. I've seen it applied to the RunInfo of a specific test.
    | Warning
    /// Test run failed, perhaps because some tests failed.
    | Failed

    /// Serialisation suitable for direct interpolation into a TRX report.
    override this.ToString () =
        match this with
        | TrxOutcome.Warning -> "Warning"
        | TrxOutcome.Completed -> "Completed"
        | TrxOutcome.Failed -> "Failed"

    /// Round-trips with ToString. Returns None if parse failed.
    static member Parse (s : string) : TrxOutcome option =
        if s.Equals ("warning", StringComparison.OrdinalIgnoreCase) then
            Some TrxOutcome.Warning
        elif s.Equals ("failed", StringComparison.OrdinalIgnoreCase) then
            Some TrxOutcome.Failed
        elif s.Equals ("completed", StringComparison.OrdinalIgnoreCase) then
            Some TrxOutcome.Completed
        else
            None

/// Ancillary information about text emitted during a complete test run.
[<NoComparison>]
type TrxRunInfo =
    {
        /// The computer on which this text was emitted.
        ComputerName : string
        /// Dunno why this field is here; it seems to be "Warning" when text is emitted?
        Outcome : TrxOutcome
        /// When this text was emitted.
        Timestamp : DateTimeOffset
        /// The text which was emitted in this event within the run.
        Text : string
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("RunInfo", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "computerName"
            attr.Value <- this.ComputerName
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "outcome"
            attr.Value <- this.Outcome.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "timestamp"
            attr.Value <- this.Timestamp.ToString "o"
            node.Attributes.Append attr |> ignore<XmlAttribute>

        let childNode = doc.CreateElement ("Text", XmlUtil.NS)
        let textNode = doc.CreateTextNode this.Text
        childNode.AppendChild textNode |> ignore<XmlNode>
        node.AppendChild childNode |> ignore<XmlNode>
        node

    static member internal ofXml (node : XmlNode) : Result<TrxRunInfo, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let computerName =
            match attrs.TryGetValue "computerName" with
            | false, _ -> Error "Expected computerName attribute"
            | true, v -> Ok v

        let outcome =
            match attrs.TryGetValue "outcome" with
            | false, _ -> Error "Expected outcome attribute"
            | true, v ->
                match TrxOutcome.Parse v with
                | Some v -> Ok v
                | None -> Error $"Could not parse '%s{v}' as an Outcome"

        let timestamp =
            match attrs.TryGetValue "timestamp" with
            | false, _ -> Error "Expected timestamp attribute"
            | true, v ->
                match DateTimeOffset.TryParse v with
                | false, _ -> Error $"Could not parse as a timestamp: %s{v}"
                | true, v -> Ok v

        let text =
            match node with
            | NodeWithNamedChild "Text" n ->
                match n with
                | OneChildNode "Text" (NoChildrenNode n) -> Ok n
                | _ -> Error "Text node unexpectedly had children"
            | _ -> Error "Expected a Text node, but found none"

        let errors =
            [
                computerName |> Result.getError
                outcome |> Result.getError
                timestamp |> Result.getError
                text |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let computerName = computerName |> Result.get |> Option.get
        let outcome = outcome |> Result.get |> Option.get
        let timestamp = timestamp |> Result.get |> Option.get
        let text = text |> Result.get |> Option.get

        {
            ComputerName = computerName
            Outcome = outcome
            Timestamp = timestamp
            Text = text
        }
        |> Ok

/// Summary statistic of what happened to all the tests in the run.
[<NoComparison>]
type TrxCounters =
    {
        /// All the tests known to the runner.
        Total : uint
        /// All the tests which were executed (e.g. skipping out [<Ignore>] ones).
        Executed : uint
        /// The tests which passed.
        Passed : uint
        /// The tests which failed.
        Failed : uint
        /// The tests which experienced an error which prevented them from terminating correctly.
        Errors : uint
        /// The tests which timed out before they could pass or fail.
        Timeout : uint
        /// Tests where the runner crashed, I think.
        Aborted : uint
        /// Tests which terminated with Assert.Inconclusive
        Inconclusive : uint
        /// Wonder when this can happen
        PassedButRunAborted : uint
        /// Dunno!
        NotRunnable : uint
        /// Search me!
        NotExecuted : uint
        /// No idea!
        Disconnected : uint
        /// Tests which succeeded but warned.
        Warning : uint
        /// I *think* this describes how many tests have been completed *for a run which is still going*.
        /// When a run's finished, this is 0.
        Completed : uint
        /// Tests which are still running.
        InProgress : uint
        /// Tests which are waiting to run.
        Pending : uint
    }

    /// Create a new Counters with one more Passed test.
    member this.AddPassed () =
        { this with
            Passed = this.Passed + 1u
            Total = this.Total + 1u
            Executed = this.Executed + 1u
        }

    /// Create a new Counters with one more Inconclusive test.
    member this.AddInconclusive () =
        { this with
            Inconclusive = this.Inconclusive + 1u
            Total = this.Total + 1u
            Executed = this.Executed + 1u
        }

    /// Create a new Counters with one more NotExecuted test.
    member this.AddNotExecuted () =
        { this with
            NotExecuted = this.NotExecuted + 1u
            Total = this.Total + 1u
        }

    /// Create a new Counters with one more Failed test.
    member this.AddFailed () =
        { this with
            Executed = this.Executed + 1u
            Total = this.Total + 1u
            Failed = this.Failed + 1u
        }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("Counters", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "total"
            attr.Value <- this.Total.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "executed"
            attr.Value <- this.Executed.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "passed"
            attr.Value <- this.Passed.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "failed"
            attr.Value <- this.Failed.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "error"
            attr.Value <- this.Errors.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "timeout"
            attr.Value <- this.Timeout.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "aborted"
            attr.Value <- this.Aborted.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "inconclusive"
            attr.Value <- this.Inconclusive.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "passedButRunAborted"
            attr.Value <- this.PassedButRunAborted.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "notRunnable"
            attr.Value <- this.NotRunnable.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "notExecuted"
            attr.Value <- this.NotExecuted.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "disconnected"
            attr.Value <- this.Disconnected.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "warning"
            attr.Value <- this.Warning.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "completed"
            attr.Value <- this.Completed.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "inProgress"
            attr.Value <- this.InProgress.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "pending"
            attr.Value <- this.Pending.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxCounters, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        // strap in, boys - thank the Lord, or possibly Bram, for Vim macros

        let total =
            match attrs.TryGetValue "total" with
            | false, _ -> Error "Expected total attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let executed =
            match attrs.TryGetValue "executed" with
            | false, _ -> Error "Expected executed attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let passed =
            match attrs.TryGetValue "passed" with
            | false, _ -> Error "Expected passed attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let failed =
            match attrs.TryGetValue "failed" with
            | false, _ -> Error "Expected failed attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let errors =
            match attrs.TryGetValue "error" with
            | false, _ -> Error "Expected error attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let timeout =
            match attrs.TryGetValue "timeout" with
            | false, _ -> Error "Expected timeout attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let aborted =
            match attrs.TryGetValue "aborted" with
            | false, _ -> Error "Expected aborted attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let inconclusive =
            match attrs.TryGetValue "inconclusive" with
            | false, _ -> Error "Expected inconclusive attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let passedButRunAborted =
            match attrs.TryGetValue "passedButRunAborted" with
            | false, _ -> Error "Expected passedButRunAborted attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let notRunnable =
            match attrs.TryGetValue "notRunnable" with
            | false, _ -> Error "Expected notRunnable attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let notExecuted =
            match attrs.TryGetValue "notExecuted" with
            | false, _ -> Error "Expected notExecuted attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let disconnected =
            match attrs.TryGetValue "disconnected" with
            | false, _ -> Error "Expected disconnected attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let warning =
            match attrs.TryGetValue "warning" with
            | false, _ -> Error "Expected warning attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let completed =
            match attrs.TryGetValue "completed" with
            | false, _ -> Error "Expected completed attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let inProgress =
            match attrs.TryGetValue "inProgress" with
            | false, _ -> Error "Expected inProgress attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let pending =
            match attrs.TryGetValue "pending" with
            | false, _ -> Error "Expected pending attribute"
            | true, v ->
                match UInt32.TryParse v with
                | true, v -> Ok v
                | false, _ -> Error $"Could not parse integer: %s{v}"

        let parseErrors =
            [
                total |> Result.getError
                executed |> Result.getError
                passed |> Result.getError
                failed |> Result.getError
                errors |> Result.getError
                timeout |> Result.getError
                aborted |> Result.getError
                inconclusive |> Result.getError
                passedButRunAborted |> Result.getError
                notRunnable |> Result.getError
                notExecuted |> Result.getError
                disconnected |> Result.getError
                warning |> Result.getError
                completed |> Result.getError
                inProgress |> Result.getError
                pending |> Result.getError
            ]
            |> List.choose id

        if not parseErrors.IsEmpty then
            parseErrors |> String.concat "; " |> Error
        else

        let total = total |> Result.get |> Option.get
        let executed = executed |> Result.get |> Option.get
        let passed = passed |> Result.get |> Option.get
        let failed = failed |> Result.get |> Option.get
        let errors = errors |> Result.get |> Option.get
        let timeout = timeout |> Result.get |> Option.get
        let aborted = aborted |> Result.get |> Option.get
        let inconclusive = inconclusive |> Result.get |> Option.get
        let passedButRunAborted = passedButRunAborted |> Result.get |> Option.get
        let notRunnable = notRunnable |> Result.get |> Option.get
        let notExecuted = notExecuted |> Result.get |> Option.get
        let disconnected = disconnected |> Result.get |> Option.get
        let warning = warning |> Result.get |> Option.get
        let completed = completed |> Result.get |> Option.get
        let inProgress = inProgress |> Result.get |> Option.get
        let pending = pending |> Result.get |> Option.get

        {
            Total = total
            Executed = executed
            Passed = passed
            Failed = failed
            Errors = errors
            Timeout = timeout
            Aborted = aborted
            Inconclusive = inconclusive
            PassedButRunAborted = passedButRunAborted
            NotRunnable = notRunnable
            NotExecuted = notExecuted
            Disconnected = disconnected
            Warning = warning
            Completed = completed
            InProgress = inProgress
            Pending = pending
        }
        |> Ok


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

/// Summary of the results; in general this doesn't explicitly mention specific test results
/// unless they have some unusual property like "printed to stdout".
[<NoComparison>]
type TrxResultsSummary =
    {
        /// Aggregated outcome of the entire test run.
        Outcome : TrxOutcome
        /// Summary of the number of tests in each possible state.
        Counters : TrxCounters
        /// Any messages emitted by the test runner itself, not by the tests.
        Output : TrxOutput
        /// Further information about some specific tests which were run.
        RunInfos : TrxRunInfo list
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("ResultSummary", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "outcome"
            attr.Value <- this.Outcome.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node.AppendChild (this.Counters.toXml doc) |> ignore<XmlNode>
        node.AppendChild (this.Output.toXml doc) |> ignore<XmlNode>

        do
            let runInfosNode = doc.CreateElement ("RunInfos", XmlUtil.NS)

            for runInfo in this.RunInfos do
                runInfosNode.AppendChild (runInfo.toXml doc) |> ignore<XmlNode>

            node.AppendChild runInfosNode |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxResultsSummary, string> =
        match node.Attributes.["outcome"] with
        | null -> Error "Expected outcome node"
        | outcome ->

        match TrxOutcome.Parse outcome.Value with
        | None -> Error $"Could not parse outcome attribute: %s{outcome.Value}"
        | Some outcome ->

        let counters =
            match node with
            | NodeWithNamedChild "Counters" n -> TrxCounters.ofXml n
            | _ -> Error "Expected Counters node on ResultsSummary"

        let output =
            match node with
            | NodeWithNamedChild "Output" n -> TrxOutput.ofXml n
            | _ -> Error "Expected Output node on ResultsSummary"

        let runInfos =
            match node with
            | NodeWithNamedChild "RunInfos" v ->
                v.ChildNodes
                |> Seq.cast
                |> Seq.map TrxRunInfo.ofXml
                |> Seq.toList
                |> Result.allOkOrError
                |> Result.mapError (fun (_, errors) -> String.concat "; " errors)
            | _ -> Error "Expected Counters node on ResultsSummary"

        let errors =
            [
                counters |> Result.getError
                runInfos |> Result.getError
                output |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let counters = counters |> Result.get |> Option.get
        let runInfos = runInfos |> Result.get |> Option.get
        let output = output |> Result.get |> Option.get

        {
            Outcome = outcome
            Counters = counters
            RunInfos = runInfos
            Output = output
        }
        |> Ok

/// A report on a test run.
[<NoComparison>]
type TrxReport =
    {
        /// An ID for this test run; this probably won't appear anywhere else in the report,
        /// it's to discriminate between *different* runs.
        Id : Guid
        /// A human-readable name for this run.
        Name : string
        /// Information about when the test run entered certain states.
        Times : TrxReportTimes
        /// Some rather inscrutable information about the configuration of this run.
        Settings : TrxTestSettings
        /// Results of all individual tests.
        Results : TrxUnitTestResult list
        /// Metadata about each individual test.
        TestDefinitions : TrxUnitTest list
        /// Mapping telling you which execution and test list each test belongs to.
        TestEntries : TrxTestEntry list
        /// It's possible for tests to be grouped into "lists"; this is metadata about those lists.
        TestLists : TrxTestListEntry list
        /// Summary information about the test run's overall status.
        ResultsSummary : TrxResultsSummary
    }

    member internal this.toXml (doc : XmlDocument) : XmlNode =
        let node = doc.CreateElement ("TestRun", XmlUtil.NS)

        do
            let attr = doc.CreateAttribute "id"
            attr.Value <- this.Id.ToString ()
            node.Attributes.Append attr |> ignore<XmlAttribute>

        do
            let attr = doc.CreateAttribute "name"
            attr.Value <- this.Name
            node.Attributes.Append attr |> ignore<XmlAttribute>

        node.AppendChild (this.Times.toXml doc) |> ignore<XmlNode>
        node.AppendChild (this.Settings.toXml doc) |> ignore<XmlNode>

        do
            let resultNode = doc.CreateElement ("Results", XmlUtil.NS)

            for result in this.Results do
                resultNode.AppendChild (result.toXml doc) |> ignore<XmlNode>

            node.AppendChild resultNode |> ignore<XmlNode>

        do
            let defsNode = doc.CreateElement ("TestDefinitions", XmlUtil.NS)

            for result in this.TestDefinitions do
                defsNode.AppendChild (result.toXml doc) |> ignore<XmlNode>

            node.AppendChild defsNode |> ignore<XmlNode>

        do
            let testsNode = doc.CreateElement ("TestEntries", XmlUtil.NS)

            for result in this.TestEntries do
                testsNode.AppendChild (result.toXml doc) |> ignore<XmlNode>

            node.AppendChild testsNode |> ignore<XmlNode>

        do
            let listsNode = doc.CreateElement ("TestLists", XmlUtil.NS)

            for result in this.TestLists do
                listsNode.AppendChild (result.toXml doc) |> ignore<XmlNode>

            node.AppendChild listsNode |> ignore<XmlNode>

        do node.AppendChild (this.ResultsSummary.toXml doc) |> ignore<XmlNode>

        node

    static member internal ofXml (node : XmlNode) : Result<TrxReport, string> =
        let attrs =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun attr -> attr.Name, attr.Value)
            |> Map.ofSeq // silently ignore duplicates

        let name =
            match attrs.TryGetValue "name" with
            | false, _ -> Error "Expected name attribute"
            | true, v -> Ok v

        let ident =
            match attrs.TryGetValue "id" with
            | false, _ -> Error "Expected id attribute"
            | true, v ->
                match Guid.TryParse v with
                | false, _ -> Error $"Could not parse GUID: %s{v}"
                | true, v -> Ok v

        let times =
            match node with
            | NodeWithNamedChild "Times" n -> TrxReportTimes.ofXml n
            | _ -> Error "Expected Times node on TrxReport"

        let testSettings =
            match node with
            | NodeWithNamedChild "TestSettings" n -> TrxTestSettings.ofXml n
            | _ -> Error "Expected TestSettings node on TrxReport"

        let resultSummary =
            match node with
            | NodeWithNamedChild "ResultSummary" n -> TrxResultsSummary.ofXml n
            | _ -> Error "Expected ResultSummary node on TrxReport"

        let testEntries =
            match node with
            | NodeWithNamedChild "TestEntries" v ->
                v.ChildNodes
                |> Seq.cast
                |> Seq.map TrxTestEntry.ofXml
                |> Seq.toList
                |> Result.allOkOrError
                |> Result.mapError (fun (_, errors) -> String.concat "; " errors)
            | _ -> Error "Expected TestEntries node on TrxReport"

        let testLists =
            match node with
            | NodeWithNamedChild "TestLists" v ->
                v.ChildNodes
                |> Seq.cast
                |> Seq.map TrxTestListEntry.ofXml
                |> Seq.toList
                |> Result.allOkOrError
                |> Result.mapError (fun (_, errors) -> String.concat "; " errors)
            | _ -> Error "Expected TestLists node on TrxReport"

        let testDefinitions =
            match node with
            | NodeWithNamedChild "TestDefinitions" v ->
                v.ChildNodes
                |> Seq.cast
                |> Seq.map TrxUnitTest.ofXml
                |> Seq.toList
                |> Result.allOkOrError
                |> Result.mapError (fun (_, errors) -> String.concat "; " errors)
            | _ -> Error "Expected TestDefinitions node on TrxReport"

        let testResults =
            match node with
            | NodeWithNamedChild "Results" v ->
                v.ChildNodes
                |> Seq.cast
                |> Seq.map TrxUnitTestResult.ofXml
                |> Seq.toList
                |> Result.allOkOrError
                |> Result.mapError (fun (_, errors) -> String.concat "; " errors)
            | _ -> Error "Expected Results node on TrxReport"

        let errors =
            [
                name |> Result.getError
                ident |> Result.getError
                times |> Result.getError
                testSettings |> Result.getError
                resultSummary |> Result.getError
                testEntries |> Result.getError
                testLists |> Result.getError
                testDefinitions |> Result.getError
                testResults |> Result.getError
            ]
            |> List.choose id

        if not errors.IsEmpty then
            errors |> String.concat "; " |> Error
        else

        let name = name |> Result.get |> Option.get
        let ident = ident |> Result.get |> Option.get
        let times = times |> Result.get |> Option.get
        let testSettings = testSettings |> Result.get |> Option.get
        let resultSummary = resultSummary |> Result.get |> Option.get
        let testEntries = testEntries |> Result.get |> Option.get
        let testLists = testLists |> Result.get |> Option.get
        let testDefinitions = testDefinitions |> Result.get |> Option.get
        let testResults = testResults |> Result.get |> Option.get

        {
            Id = ident
            Name = name
            Times = times
            Settings = testSettings
            Results = testResults
            TestDefinitions = testDefinitions
            TestEntries = testEntries
            TestLists = testLists
            ResultsSummary = resultSummary
        }
        |> Ok

/// A report on a test run.
[<RequireQualifiedAccess>]
module TrxReport =
    /// Parse the report. May instead return a human-readable description of why we couldn't parse.
    let parse (s : string) : Result<TrxReport, string> =
        let node = XmlDocument ()
        node.LoadXml s

        match node with
        | NodeWithNamedChild "TestRun" node -> TrxReport.ofXml node
        | _ -> Error "XML document did not have a TestRun node"

    /// Render this report as XML, suitable to save in a .TRX file.
    let toXml (report : TrxReport) : XmlDocument =
        let doc = XmlDocument ()

        doc.AppendChild (doc.CreateXmlDeclaration ("1.0", "utf-8", ""))
        |> ignore<XmlNode>

        doc.AppendChild (report.toXml doc) |> ignore<XmlNode>
        doc
