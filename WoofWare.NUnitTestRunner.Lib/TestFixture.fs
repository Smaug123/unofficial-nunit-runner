namespace WoofWare.NUnitTestRunner

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open Microsoft.FSharp.Core

type private StdoutSetter (newStdout : StreamWriter, newStderr : StreamWriter) =
    let oldStdout = Console.Out
    let oldStderr = Console.Error

    do
        Console.SetOut newStdout
        Console.SetError newStderr

    interface IDisposable with
        member _.Dispose () =
            Console.SetOut oldStdout
            Console.SetError oldStderr

/// Information about the circumstances of a run of a single test.
type IndividualTestRunMetadata =
    {
        /// How long the test took.
        Total : TimeSpan
        /// When the test started.
        Start : DateTimeOffset
        /// When the test ended.
        End : DateTimeOffset
        /// The Environment.MachineName of the computer on which the run happened.
        ComputerName : string
        /// An identifier for this run of this test.
        ExecutionId : Guid
        /// An identifier for this test (possibly shared across repeats of this exact test with the same args).
        TestId : Guid
        /// Human-readable string representing this individual single test run, including any parameters.
        TestName : string
        /// Name of the class from which this test derived
        ClassName : string
        /// Anything that was printed to stdout while the test ran.
        StdOut : string option
        /// Anything that was printed to stderr while the test ran.
        StdErr : string option
    }

/// The results of running a single TestFixture.
type FixtureRunResults =
    {
        /// These tests failed.
        /// TODO: domain is squiffy, the TestMemberFailure wants to be instead a TestFailure
        Failed : (TestMemberFailure * IndividualTestRunMetadata) list
        /// These tests succeeded.
        /// A given test method may appear many times in this list, if it represented many tests.
        Success : (SingleTestMethod * TestMemberSuccess * IndividualTestRunMetadata) list
        /// These failures occurred outside the context of a test - e.g. in setup or tear-down logic.
        OtherFailures : (UserMethodFailure * IndividualTestRunMetadata) list
    }

    /// Another view on the data contained in this object, transposed.
    member this.IndividualTestRunMetadata
        : (IndividualTestRunMetadata * Choice<TestMemberFailure, TestMemberSuccess, UserMethodFailure>) list =
        [
            for a, d in this.Failed do
                yield d, Choice1Of3 a
            for _, a, d in this.Success do
                yield d, Choice2Of3 a
            for a, d in this.OtherFailures do
                yield d, Choice3Of3 a
        ]

/// A test fixture (usually represented by the [<TestFixture>]` attribute), which may contain many tests,
/// each of which may run many times.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TestFixture =
    /// It's possible for multiple things to fail about a test: e.g. the test failed and also the tear-down failed.
    ///
    /// This function does not throw.
    let private runOne
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (testId : Guid)
        (test : MethodInfo)
        (containingObject : obj)
        (args : obj[])
        : Result<TestMemberSuccess, TestFailure list> * IndividualTestRunMetadata
        =
        let rec runMethods
            (wrap : UserMethodFailure -> TestFailure)
            (toRun : MethodInfo list)
            (args : obj[])
            : Result<unit, _>
            =
            match toRun with
            | [] -> Ok ()
            | head :: rest ->
                let result =
                    try
                        head.Invoke (containingObject, args) |> Ok
                    with :? TargetInvocationException as e ->
                        Error (UserMethodFailure.Threw (head.Name, e.InnerException))

                match result with
                | Error e -> Error (wrap e)
                | Ok result ->
                    match result with
                    | :? unit -> runMethods wrap rest args
                    | ret -> UserMethodFailure.ReturnedNonUnit (head.Name, ret) |> wrap |> Error

        let start = DateTimeOffset.Now

        use stdOutStream = new MemoryStream ()
        use stdErrStream = new MemoryStream ()
        use stdOut = new StreamWriter (stdOutStream)
        use stdErr = new StreamWriter (stdErrStream)

        use _ = new StdoutSetter (stdOut, stdErr)

        let sw = Stopwatch.StartNew ()

        let metadata () =
            let name =
                if args.Length = 0 then
                    test.Name
                else
                    let argsStr = args |> Seq.map string<obj> |> String.concat ","
                    $"%s{test.Name}(%s{argsStr})"

            {
                End = DateTimeOffset.Now
                Start = start
                Total = sw.Elapsed
                ComputerName = Environment.MachineName
                ExecutionId = Guid.NewGuid ()
                TestId = testId
                TestName = name
                ClassName = test.DeclaringType.FullName
                StdOut =
                    match stdOutStream.ToArray () with
                    | [||] -> None
                    | arr -> Console.OutputEncoding.GetString arr |> Some
                StdErr =
                    match stdErrStream.ToArray () with
                    | [||] -> None
                    | arr -> Console.OutputEncoding.GetString arr |> Some
            }

        let setUpResult = runMethods TestFailure.SetUpFailed setUp [||]
        sw.Stop ()

        match setUpResult with
        | Error e -> Error [ e ], metadata ()
        | Ok () ->

        sw.Start ()

        let result =
            let result = runMethods TestFailure.TestFailed [ test ] args
            sw.Stop ()

            match result with
            | Ok () -> Ok None
            | Error (TestFailure.TestFailed (UserMethodFailure.Threw (_, exc)) as orig) ->
                match exc.GetType().FullName with
                | "NUnit.Framework.SuccessException" -> Ok None
                | "NUnit.Framework.IgnoreException" -> Ok (Some (TestMemberSuccess.Ignored (Option.ofObj exc.Message)))
                | "NUnit.Framework.InconclusiveException" ->
                    Ok (Some (TestMemberSuccess.Inconclusive (Option.ofObj exc.Message)))
                | s when s.StartsWith ("NUnit.Framework.", StringComparison.Ordinal) ->
                    failwith $"Unrecognised special exception: %s{s}"
                | _ -> Error orig
            | Error orig -> Error orig

        // Unconditionally run TearDown after tests, even if tests failed.
        sw.Start ()
        let tearDownResult = runMethods TestFailure.TearDownFailed tearDown [||]
        sw.Stop ()

        let metadata = metadata ()

        match result, tearDownResult with
        | Ok None, Ok () -> Ok TestMemberSuccess.Ok, metadata
        | Ok (Some s), Ok () -> Ok s, metadata
        | Error e, Ok ()
        | Ok _, Error e -> Error [ e ], metadata
        | Error e1, Error e2 -> Error [ e1 ; e2 ], metadata

    let private getValues (test : SingleTestMethod) =
        let valuesAttrs =
            test.Method.GetParameters ()
            |> Array.map (fun i ->
                i.CustomAttributes
                |> Seq.choose (fun i ->
                    if i.AttributeType.FullName = "NUnit.Framework.ValuesAttribute" then
                        Some i.ConstructorArguments
                    else
                        None
                )
                |> Seq.toList
                |> function
                    | [] -> Ok None
                    | [ x ] -> Ok (Some x)
                    | _ :: _ :: _ ->
                        "Multiple Values attributes on a parameter. Exactly one per parameter please."
                        |> Error
            )
            |> Array.allOkOrError

        match valuesAttrs with
        | Error (_, e) -> Error (TestMemberFailure.Malformed (List.ofArray e))
        | Ok valuesAttrs ->

        if valuesAttrs |> Array.exists (fun l -> l.IsSome) then
            if valuesAttrs |> Array.exists (fun l -> l.IsNone) then
                failwith
                    $"Test '%s{test.Name}' has a parameter with the Values attribute and a parameter without. All parameters must have Values if any one does."

            Some (valuesAttrs |> Array.map Option.get) |> Ok
        else
            Ok None

    /// This method should never throw: it only throws if there's a critical logic error in the runner.
    /// Exceptions from the units under test are wrapped up and passed out.
    let private runTestsFromMember
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (containingObject : obj)
        (test : SingleTestMethod)
        : (Result<TestMemberSuccess, TestMemberFailure> * IndividualTestRunMetadata) list
        =
        if test.Method.ContainsGenericParameters then
            let failureMetadata =
                {
                    Total = TimeSpan.Zero
                    Start = DateTimeOffset.Now
                    End = DateTimeOffset.Now
                    ComputerName = Environment.MachineName
                    ExecutionId = Guid.NewGuid ()
                    TestId = Guid.NewGuid ()
                    TestName = test.Name
                    ClassName = test.Method.DeclaringType.FullName
                    StdErr = None
                    StdOut = None
                }

            let error =
                TestMemberFailure.Malformed [ "Test contained generic parameters; generics are not supported." ]

            (Error error, failureMetadata) |> List.singleton
        else

        let resultPreRun =
            (None, test.Modifiers)
            ||> List.fold (fun _result modifier ->
                // TODO: would be nice not to throw away the accumulation,
                // and also when we get to being able to run Explicit tests we should discriminate exactly whether
                // there was an Ignore
                match modifier with
                | Modifier.Explicit reason ->
                    // TODO: have a mode where we can run explicit tests
                    Some (TestMemberSuccess.Explicit reason)
                | Modifier.Ignored reason -> Some (TestMemberSuccess.Ignored reason)
            )

        let sw = Stopwatch.StartNew ()
        let startTime = DateTimeOffset.Now

        match resultPreRun with
        | Some result ->
            sw.Stop ()

            let failureMetadata =
                {
                    Total = sw.Elapsed
                    Start = startTime
                    End = DateTimeOffset.Now
                    ComputerName = Environment.MachineName
                    ExecutionId = Guid.NewGuid ()
                    // No need to keep these test GUIDs stable: no point trying to run an explicit test multiple times.
                    TestId = Guid.NewGuid ()
                    TestName = test.Name
                    ClassName = test.Method.DeclaringType.FullName
                    StdErr = None
                    StdOut = None
                }

            [ Ok result, failureMetadata ]
        | None ->

        let individualTests =
            let values = getValues test

            match values with
            | Error e -> Error e
            | Ok values ->
                match test.Kind, values with
                | TestKind.Data data, None -> data |> List.map (fun args -> Guid.NewGuid (), Array.ofList args) |> Ok
                | TestKind.Data _, Some _ ->
                    [
                        "Test has both the TestCase and Values attributes. Specify one or the other."
                    ]
                    |> TestMemberFailure.Malformed
                    |> Error
                | TestKind.Single, None -> (Guid.NewGuid (), [||]) |> List.singleton |> Ok
                | TestKind.Single, Some vals ->
                    let combinatorial =
                        Option.defaultValue Combinatorial.Combinatorial test.Combinatorial

                    match combinatorial with
                    | Combinatorial.Combinatorial ->
                        vals
                        |> Seq.map (fun l -> l |> Seq.map (fun v -> v.Value) |> Seq.toList)
                        |> Seq.toList
                        |> List.combinations
                        |> List.map (fun args -> Guid.NewGuid (), Array.ofList args)
                        |> Ok
                    | Combinatorial.Sequential ->
                        let maxLength = vals |> Seq.map (fun i -> i.Count) |> Seq.max

                        List.init
                            maxLength
                            (fun i ->
                                let args =
                                    vals
                                    |> Array.map (fun param -> if i >= param.Count then null else param.[i].Value)

                                Guid.NewGuid (), args
                            )
                        |> Ok
                | TestKind.Source _, Some _ ->
                    [
                        "Test has both the TestCaseSource and Values attributes. Specify one or the other."
                    ]
                    |> TestMemberFailure.Malformed
                    |> Error
                | TestKind.Source sources, None ->
                    [
                        for source in sources do
                            let args =
                                test.Method.DeclaringType.GetProperty (
                                    source,
                                    BindingFlags.Public
                                    ||| BindingFlags.NonPublic
                                    ||| BindingFlags.Instance
                                    ||| BindingFlags.Static
                                )

                            // Might not be an IEnumerable of a reference type.
                            // Concretely, `FSharpList<HttpStatusCode> :> IEnumerable<obj>` fails.
                            for arg in args.GetValue (null : obj) :?> System.Collections.IEnumerable do
                                yield
                                    Guid.NewGuid (),
                                    match arg with
                                    | null -> [| (null : obj) |]
                                    | :? Tuple<obj, obj> as (a, b) -> [| a ; b |]
                                    | :? Tuple<obj, obj, obj> as (a, b, c) -> [| a ; b ; c |]
                                    | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) -> [| a ; b ; c ; d |]
                                    | arg ->
                                        let argTy = arg.GetType ()

                                        if argTy.FullName = "NUnit.Framework.TestCaseData" then
                                            let argsMem =
                                                argTy.GetMethod (
                                                    "get_Arguments",
                                                    BindingFlags.Public
                                                    ||| BindingFlags.Instance
                                                    ||| BindingFlags.FlattenHierarchy
                                                )

                                            if isNull argsMem then
                                                failwith "Unexpectedly could not call `.Arguments` on TestCaseData"

                                            (argsMem.Invoke (arg, [||]) |> unbox<obj[]>)
                                        else
                                            [| arg |]
                    ]
                    |> Ok

        sw.Stop ()

        match individualTests with
        | Error e ->
            let failureMetadata =
                {
                    Total = sw.Elapsed
                    Start = startTime
                    End = DateTimeOffset.Now
                    ComputerName = Environment.MachineName
                    ExecutionId = Guid.NewGuid ()
                    // No need to keep these test GUIDs stable: we're not going to run them multiple times,
                    // because we're not going to run anything at all.
                    TestId = Guid.NewGuid ()
                    TestName = test.Name
                    ClassName = test.Method.DeclaringType.FullName
                    StdErr = None
                    StdOut = None
                }

            [ Error e, failureMetadata ]
        | Ok individualTests ->

        let count = test.Repeat |> Option.defaultValue 1

        Seq.init count (fun _ -> individualTests)
        |> Seq.concat
        |> Seq.map (fun (testGuid, args) ->
            let results, summary =
                runOne setUp tearDown testGuid test.Method containingObject args

            match results with
            | Ok results -> Ok results, summary
            | Error e -> Error (TestMemberFailure.Failed e), summary
        )
        |> Seq.toList

    /// Run every test (except those which fail the `filter`) in this test fixture, as well as the
    /// appropriate setup and tear-down logic.
    let run
        (progress : ITestProgress)
        (filter : TestFixture -> SingleTestMethod -> bool)
        (tests : TestFixture)
        : FixtureRunResults
        =
        progress.OnTestFixtureStart tests.Name tests.Tests.Length

        let containingObject =
            let methods =
                seq {
                    match tests.OneTimeSetUp with
                    | None -> ()
                    | Some t -> yield t

                    match tests.OneTimeTearDown with
                    | None -> ()
                    | Some t -> yield t

                    yield! tests.Tests |> Seq.map (fun t -> t.Method)
                }

            methods
            |> Seq.tryPick (fun mi ->
                if not mi.IsStatic then
                    Some (Activator.CreateInstance mi.DeclaringType)
                else
                    None
            )
            |> Option.toObj

        let oldWorkDir = Environment.CurrentDirectory
        Environment.CurrentDirectory <- FileInfo(tests.ContainingAssembly.Location).Directory.FullName

        let sw = Stopwatch.StartNew ()
        let startTime = DateTimeOffset.UtcNow

        use stdOutStream = new MemoryStream ()
        use stdOut = new StreamWriter (stdOutStream)
        use stdErrStream = new MemoryStream ()
        use stdErr = new StreamWriter (stdErrStream)
        use _ = new StdoutSetter (stdOut, stdErr)

        let endMetadata () =
            let stdOut = stdOutStream.ToArray () |> Console.OutputEncoding.GetString
            let stdErr = stdErrStream.ToArray () |> Console.OutputEncoding.GetString

            {
                Total = sw.Elapsed
                Start = startTime
                End = DateTimeOffset.UtcNow
                ComputerName = Environment.MachineName
                ExecutionId = Guid.NewGuid ()
                TestId = Guid.NewGuid ()
                // This one is a bit dubious, because we don't actually have a test name at all
                TestName = tests.Name
                ClassName = tests.Name
                StdOut = if String.IsNullOrEmpty stdOut then None else Some stdOut
                StdErr = if String.IsNullOrEmpty stdErr then None else Some stdErr
            }

        let setupResult =
            match tests.OneTimeSetUp with
            | Some su ->
                try
                    match su.Invoke (containingObject, [||]) with
                    | :? unit -> None
                    | ret -> Some (UserMethodFailure.ReturnedNonUnit (su.Name, ret), endMetadata ())
                with :? TargetInvocationException as e ->
                    Some (UserMethodFailure.Threw (su.Name, e.InnerException), endMetadata ())
            | _ -> None

        let testFailures = ResizeArray<TestMemberFailure * IndividualTestRunMetadata> ()

        let successes =
            ResizeArray<SingleTestMethod * TestMemberSuccess * IndividualTestRunMetadata> ()

        match setupResult with
        | Some _ ->
            // Don't run any tests if setup failed.
            ()
        | None ->
            for test in tests.Tests do
                if filter tests test then
                    progress.OnTestMemberStart test.Name
                    let testSuccess = ref 0

                    let results = runTestsFromMember tests.SetUp tests.TearDown containingObject test

                    for result, report in results do
                        match result with
                        | Error failure ->
                            testFailures.Add (failure, report)
                            progress.OnTestFailed test.Name failure
                        | Ok result ->
                            Interlocked.Increment testSuccess |> ignore<int>
                            lock successes (fun () -> successes.Add (test, result, report))

                    progress.OnTestMemberFinished test.Name
                else
                    progress.OnTestMemberSkipped test.Name

        // Unconditionally run OneTimeTearDown if it exists.
        let tearDownError =
            match tests.OneTimeTearDown with
            | Some td ->
                try
                    match td.Invoke (containingObject, [||]) with
                    | null -> None
                    | ret -> Some (UserMethodFailure.ReturnedNonUnit (td.Name, ret), endMetadata ())
                with :? TargetInvocationException as e ->
                    Some (UserMethodFailure.Threw (td.Name, e), endMetadata ())
            | _ -> None

        Environment.CurrentDirectory <- oldWorkDir

        {
            Failed = testFailures |> Seq.toList
            Success = successes |> Seq.toList
            OtherFailures = [ tearDownError ; setupResult ] |> List.choose id
        }

    /// Interpret this type as a [<TestFixture>], extracting the test members from it and annotating them with all
    /// relevant information about how we should run them.
    let parse (parentType : Type) : TestFixture =
        let categories =
            parentType.CustomAttributes
            |> Seq.filter (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.CategoryAttribute")
            |> Seq.map (fun attr -> attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>)
            |> Seq.toList

        (TestFixture.Empty parentType.Assembly parentType.Name, parentType.GetRuntimeMethods ())
        ||> Seq.fold (fun state mi ->
            ((state, []), mi.CustomAttributes)
            ||> Seq.fold (fun (state, unrecognisedAttrs) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.OneTimeSetUpAttribute" ->
                    match state.OneTimeSetUp with
                    | Some _existing -> failwith "Multiple OneTimeSetUp methods found"
                    | None ->
                        { state with
                            OneTimeSetUp = Some mi
                        },
                        unrecognisedAttrs
                | "NUnit.Framework.OneTimeTearDownAttribute" ->
                    match state.OneTimeTearDown with
                    | Some _existing -> failwith "Multiple OneTimeTearDown methods found"
                    | None ->
                        { state with
                            OneTimeTearDown = Some mi
                        },
                        unrecognisedAttrs
                | "NUnit.Framework.TearDownAttribute" ->
                    { state with
                        TearDown = mi :: state.TearDown
                    },
                    unrecognisedAttrs
                | "NUnit.Framework.SetUpAttribute" ->
                    { state with
                        SetUp = mi :: state.SetUp
                    },
                    unrecognisedAttrs
                | "NUnit.Framework.TestFixtureSetUpAttribute" ->
                    failwith "TestFixtureSetUp is not supported (upstream has deprecated it; use OneTimeSetUp)"
                | "NUnit.Framework.TestFixtureTearDownAttribute" ->
                    failwith "TestFixtureTearDown is not supported (upstream has deprecated it; use OneTimeTearDown)"
                | "NUnit.Framework.RetryAttribute" ->
                    failwith "RetryAttribute is not supported. Don't write flaky tests."
                | "NUnit.Framework.RandomAttribute" ->
                    failwith "RandomAttribute is not supported. Use a property-based testing framework like FsCheck."
                | "NUnit.Framework.AuthorAttribute"
                | "NUnit.Framework.CultureAttribute"
                | "NUnit.Framework.DescriptionAttribute" ->
                    // ignoring for now: metadata only
                    state, unrecognisedAttrs
                | _ -> state, attr :: unrecognisedAttrs
            )
            |> fun (state, unrecognised) ->
                let state, unrecognised =
                    match SingleTestMethod.parse categories mi unrecognised with
                    | Some test, unrecognised ->
                        { state with
                            Tests = test :: state.Tests
                        },
                        unrecognised
                    | None, unrecognised -> state, unrecognised

                unrecognised
                |> List.filter (fun attr ->
                    attr.AttributeType.FullName.StartsWith ("NUnit.Framework.", StringComparison.Ordinal)
                )
                |> function
                    | [] -> ()
                    | unrecognised ->
                        unrecognised
                        |> Seq.map (fun x -> x.AttributeType.FullName)
                        |> String.concat ", "
                        |> failwithf "Unrecognised attributes: %s"

                state
        )
