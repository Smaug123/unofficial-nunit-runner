namespace WoofWare.NUnitTestRunner

open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core

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
        (outputId : OutputStreamId)
        (contexts : TestContexts)
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (testId : Guid)
        (test : MethodInfo)
        (containingObject : obj)
        (args : obj[])
        : Async<Result<TestMemberSuccess, TestFailure list> * IndividualTestRunMetadata>
        =
        let rec runMethods
            (wrap : UserMethodFailure -> TestFailure)
            (toRun : MethodInfo list)
            (args : obj[])
            : Result<unit, TestFailure> Async
            =
            match toRun with
            | [] -> async.Return (Ok ())
            | head :: rest ->
                async {
                    let result =
                        try
                            head.Invoke (containingObject, args) |> Ok
                        with
                        | :? TargetInvocationException as e ->
                            Error (UserMethodFailure.Threw (head.Name, e.InnerException))
                        | :? TargetParameterCountException ->
                            UserMethodFailure.BadParameters (
                                head.Name,
                                head.GetParameters () |> Array.map (fun pm -> pm.ParameterType),
                                args
                            )
                            |> Error

                    match result with
                    | Error e -> return (Error (wrap e))
                    | Ok result ->
                        match result with
                        | :? unit -> return! runMethods wrap rest args
                        | ret -> return UserMethodFailure.ReturnedNonUnit (head.Name, ret) |> wrap |> Error

                }

        async {
            let start = DateTimeOffset.Now

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
                        match contexts.DumpStdout outputId with
                        | "" -> None
                        | v -> Some v
                    StdErr =
                        match contexts.DumpStderr outputId with
                        | "" -> None
                        | v -> Some v
                }

            let! setUpResult = runMethods TestFailure.SetUpFailed setUp [||]
            sw.Stop ()

            match setUpResult with
            | Error e -> return Error [ e ], metadata ()
            | Ok () ->

            sw.Start ()
            let! result = runMethods TestFailure.TestFailed [ test ] args
            sw.Stop ()

            let result =
                match result with
                | Ok () -> Ok None
                | Error (TestFailure.TestFailed (UserMethodFailure.Threw (_, exc)) as orig) ->
                    match exc.GetType().FullName with
                    | "NUnit.Framework.SuccessException" -> Ok None
                    | "NUnit.Framework.IgnoreException" ->
                        Ok (Some (TestMemberSuccess.Ignored (Option.ofObj exc.Message)))
                    | "NUnit.Framework.InconclusiveException" ->
                        Ok (Some (TestMemberSuccess.Inconclusive (Option.ofObj exc.Message)))
                    | _ -> Error orig
                | Error orig -> Error orig

            // Unconditionally run TearDown after tests, even if tests failed.
            sw.Start ()
            let! tearDownResult = runMethods TestFailure.TearDownFailed tearDown [||]
            sw.Stop ()

            let metadata = metadata ()

            return
                match result, tearDownResult with
                | Ok None, Ok () -> Ok TestMemberSuccess.Ok, metadata
                | Ok (Some s), Ok () -> Ok s, metadata
                | Error e, Ok ()
                | Ok _, Error e -> Error [ e ], metadata
                | Error e1, Error e2 -> Error [ e1 ; e2 ], metadata
        }

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
        (contexts : TestContexts)
        (par : ParallelQueue)
        (running : TestFixtureSetupToken)
        (progress : ITestProgress)
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (containingObject : obj)
        (test : SingleTestMethod)
        : (Result<TestMemberSuccess, TestMemberFailure> * IndividualTestRunMetadata) Task list
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

            (Error error, failureMetadata) |> Task.FromResult |> List.singleton
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

        match resultPreRun with
        | Some result ->
            let failureMetadata =
                {
                    Total = TimeSpan.Zero
                    Start = DateTimeOffset.Now
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

            (Ok result, failureMetadata) |> Task.FromResult |> List.singleton
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
                            for arg in args.GetValue (null : obj) :?> IEnumerable do
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

                                            // TODO: need to capture this stdout/stderr
                                            (argsMem.Invoke (arg, [||]) |> unbox<obj[]>)
                                        else
                                            [| arg |]
                    ]
                    |> Ok

        match individualTests with
        | Error e ->
            let failureMetadata =
                {
                    Total = TimeSpan.Zero
                    Start = DateTimeOffset.Now
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

            (Error e, failureMetadata) |> Task.FromResult |> List.singleton
        | Ok individualTests ->

        let count = test.Repeat |> Option.defaultValue 1

        Seq.init count (fun _ -> individualTests)
        |> Seq.concat
        |> Seq.map (fun (testGuid, args) ->
            task {
                let runMe () =
                    async {
                        progress.OnTestMemberStart test.Name
                        let oldValue = contexts.AsyncLocal.Value
                        let outputId = contexts.NewOutputs ()
                        contexts.AsyncLocal.Value <- outputId

                        let! result, meta =
                            runOne outputId contexts setUp tearDown testGuid test.Method containingObject args

                        contexts.AsyncLocal.Value <- oldValue
                        progress.OnTestMemberFinished test.Name

                        return result, meta
                    }

                let! results, summary = par.RunAsync running test.Parallelize runMe

                match results with
                | Ok results -> return Ok results, summary
                | Error e -> return Error (TestMemberFailure.Failed e), summary
            }
        )
        |> Seq.toList

    /// Run every test (except those which fail the `filter`) in this test fixture, as well as the
    /// appropriate setup and tear-down logic.
    let runOneFixture
        (contexts : TestContexts)
        (par : ParallelQueue)
        (progress : ITestProgress)
        (filter : TestFixture -> SingleTestMethod -> bool)
        (name : string)
        (containingObject : obj)
        (tests : TestFixture)
        : FixtureRunResults Task
        =
        task {
            let! running = par.StartTestFixture tests
            progress.OnTestFixtureStart name tests.Tests.Length

            let oldWorkDir = Environment.CurrentDirectory
            Environment.CurrentDirectory <- FileInfo(tests.ContainingAssembly.Location).Directory.FullName

            let sw = Stopwatch.StartNew ()
            let startTime = DateTimeOffset.UtcNow

            let endMetadata (outputId : OutputStreamId) =
                let stdOut = contexts.DumpStdout outputId
                let stdErr = contexts.DumpStderr outputId

                {
                    Total = sw.Elapsed
                    Start = startTime
                    End = DateTimeOffset.UtcNow
                    ComputerName = Environment.MachineName
                    ExecutionId = Guid.NewGuid ()
                    TestId = Guid.NewGuid ()
                    // This one is a bit dubious, because we don't actually have a test name at all
                    TestName = name
                    ClassName = tests.Name
                    StdOut = if String.IsNullOrEmpty stdOut then None else Some stdOut
                    StdErr = if String.IsNullOrEmpty stdErr then None else Some stdErr
                }

            let! setupResult, running =
                match tests.OneTimeSetUp with
                | Some su ->
                    par.RunTestSetup
                        running
                        (fun () ->
                            let oldValue = contexts.AsyncLocal.Value
                            let newOutputs = contexts.NewOutputs ()
                            contexts.AsyncLocal.Value <- newOutputs

                            let result =
                                try
                                    match su.Invoke (containingObject, [||]) with
                                    | :? unit -> None
                                    | ret ->
                                        Some (UserMethodFailure.ReturnedNonUnit (su.Name, ret), endMetadata newOutputs)
                                with :? TargetInvocationException as e ->
                                    Some (UserMethodFailure.Threw (su.Name, e.InnerException), endMetadata newOutputs)

                            contexts.AsyncLocal.Value <- oldValue

                            match result with
                            | None -> Ok (Some newOutputs)
                            | Some err -> Error (err, newOutputs)
                        )
                | _ -> Task.FromResult (Ok None, TestFixtureSetupToken.vouchNoSetupRequired running)

            let testFailures = ResizeArray<TestMemberFailure * IndividualTestRunMetadata> ()

            let successes =
                ResizeArray<SingleTestMethod * TestMemberSuccess * IndividualTestRunMetadata> ()

            let testsRun =
                match setupResult with
                | Error _ ->
                    // Don't run any tests if setup failed.
                    Task.FromResult ()
                | Ok _ ->
                    tests.Tests
                    |> Seq.filter (fun test ->
                        if filter tests test then
                            true
                        else
                            progress.OnTestMemberSkipped test.Name
                            false
                    )
                    |> Seq.map (fun test ->
                        task {
                            let testSuccess = ref 0

                            let results =
                                runTestsFromMember
                                    contexts
                                    par
                                    running
                                    progress
                                    tests.SetUp
                                    tests.TearDown
                                    containingObject
                                    test

                            let! result =
                                results
                                |> List.map (fun t ->
                                    task {
                                        let! result, report = t

                                        match result with
                                        | Error failure ->
                                            testFailures.Add (failure, report)
                                            progress.OnTestFailed test.Name failure
                                        | Ok result ->
                                            Interlocked.Increment testSuccess |> ignore<int>
                                            lock successes (fun () -> successes.Add (test, result, report))
                                    }
                                )
                                |> Task.WhenAll

                            result |> Array.iter id
                        }
                    )
                    |> Task.WhenAll
                    |> fun t ->
                        task {
                            let! t = t
                            return t |> Array.iter id
                        }

            do! testsRun

            // Unconditionally run OneTimeTearDown if it exists.
            let! tearDownError, tornDown =
                match tests.OneTimeTearDown with
                | Some td ->
                    par.RunTestTearDown
                        running
                        (fun () ->
                            let oldValue = contexts.AsyncLocal.Value
                            let outputs = contexts.NewOutputs ()
                            contexts.AsyncLocal.Value <- outputs

                            let result =
                                try
                                    match td.Invoke (containingObject, [||]) with
                                    | :? unit -> None
                                    | ret ->
                                        Some (UserMethodFailure.ReturnedNonUnit (td.Name, ret), endMetadata outputs)
                                with :? TargetInvocationException as e ->
                                    Some (UserMethodFailure.Threw (td.Name, e.InnerException), endMetadata outputs)

                            contexts.AsyncLocal.Value <- oldValue

                            match result with
                            | None -> Ok (Some outputs)
                            | Some err -> Error (err, outputs)
                        )
                | _ -> Task.FromResult (Ok None, TestFixtureTearDownToken.vouchNoTearDownRequired running)

            Environment.CurrentDirectory <- oldWorkDir

            do! par.EndTestFixture tornDown

            // TODO: we have access to stdout/err of OneTimeSetUp and OneTimeTearDown here, but we throw them away.
            return
                {
                    Failed = testFailures |> Seq.toList
                    Success = successes |> Seq.toList
                    OtherFailures =
                        [ tearDownError ; setupResult ]
                        |> List.choose (
                            function
                            | Error (e, _) -> Some e
                            | Ok _ -> None
                        )
                }
        }

    /// Interpret this type as a [<TestFixture>], extracting the test members from it and annotating them with all
    /// relevant information about how we should run them.
    let parse (parentType : Type) : TestFixture =
        let categories, args, mods, par =
            (([], [], [], None), parentType.CustomAttributes)
            ||> Seq.fold (fun (categories, args, mods, par) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.SetUpFixtureAttribute" ->
                    failwith "This test runner does not support SetUpFixture. Please shout if you want this."
                | "NUnit.Framework.CategoryAttribute" ->
                    let cat = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>
                    cat :: categories, args, mods, par
                | "NUnit.Framework.TestFixtureAttribute" ->
                    let newArgs =
                        match attr.ConstructorArguments |> Seq.map _.Value |> Seq.toList with
                        | [ :? ICollection as x ] ->
                            x |> Seq.cast<CustomAttributeTypedArgument> |> Seq.map _.Value |> Seq.toList
                        | xs -> xs

                    categories, newArgs :: args, mods, par
                | "NUnit.Framework.NonParallelizableAttribute" ->
                    match par with
                    | Some _ -> failwith $"Got multiple parallelism attributes on %s{parentType.FullName}"
                    | None -> categories, args, mods, Some Parallelizable.No
                | "NUnit.Framework.ParallelizableAttribute" ->
                    match par with
                    | Some _ -> failwith $"Got multiple parallelism attributes on %s{parentType.FullName}"
                    | None ->
                        match attr.ConstructorArguments |> Seq.toList with
                        | [] -> categories, args, mods, Some (Parallelizable.Yes ClassParallelScope.Self)
                        | [ v ] ->
                            match v.Value with
                            | :? int as v ->
                                match ParallelScope.ofInt v with
                                | ParallelScope.Fixtures ->
                                    categories, args, mods, Some (Parallelizable.Yes ClassParallelScope.Fixtures)
                                | ParallelScope.Children ->
                                    categories, args, mods, Some (Parallelizable.Yes ClassParallelScope.Children)
                                | ParallelScope.All ->
                                    categories, args, mods, Some (Parallelizable.Yes ClassParallelScope.All)
                                | ParallelScope.Self ->
                                    categories, args, mods, Some (Parallelizable.Yes ClassParallelScope.Self)
                                | ParallelScope.None -> categories, args, mods, Some Parallelizable.No
                            | v ->
                                failwith
                                    $"Unexpectedly non-int value %O{v} of parallel scope in %s{parentType.FullName}"
                        | _ -> failwith $"unexpectedly got multiple args to Parallelizable on %s{parentType.FullName}"
                | "NUnit.Framework.ExplicitAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    categories, args, Modifier.Explicit reason :: mods, par
                | "NUnit.Framework.IgnoreAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    categories, args, Modifier.Ignored reason :: mods, par
                | _ -> categories, args, mods, par
            )

        (TestFixture.Empty parentType par mods args, parentType.GetRuntimeMethods ())
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

    /// Run every test (except those which fail the `filter`) in this test fixture, as well as the
    /// appropriate setup and tear-down logic.
    ///
    /// If the TestFixture has modifiers that specify no tests should be run, we don't run any tests.
    let run
        (contexts : TestContexts)
        (par : ParallelQueue)
        (progress : ITestProgress)
        (filter : TestFixture -> SingleTestMethod -> bool)
        (tests : TestFixture)
        : FixtureRunResults list Task
        =
        match
            tests.Modifiers
            |> List.tryFind (
                function
                | Modifier.Explicit _
                | Modifier.Ignored _ -> true
            )
        with
        | Some modifier ->
            let reason =
                match modifier with
                | Modifier.Explicit (Some reason) -> reason
                | Modifier.Ignored (Some reason) -> reason
                | Modifier.Ignored None -> "test fixture marked Ignore"
                | Modifier.Explicit None -> "test fixture marked Explicit"

            progress.OnTestFixtureSkipped tests.Name reason
            Task.FromResult []
        | None ->

        match tests.Parameters with
        | [] -> [ null ]
        | args -> args |> List.map List.toArray
        |> List.map (fun args ->
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
                        Some (Activator.CreateInstance (mi.DeclaringType, args))
                    else
                        None
                )
                |> Option.toObj

            let name =
                if isNull args then
                    tests.Name
                else
                    let args = args |> Seq.map (fun o -> o.ToString ()) |> String.concat ","
                    $"%s{tests.Name}(%s{args})"

            runOneFixture contexts par progress filter name containingObject tests
        )
        |> Task.WhenAll
        |> fun t ->
            task {
                let! t = t
                return Array.toList t
            }
