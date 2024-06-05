namespace TestRunner

open System
open System.Reflection
open System.Threading
open Microsoft.FSharp.Core

/// Represents the result of a test that didn't fail.
[<RequireQualifiedAccess>]
type TestMemberSuccess =
    /// The test passed.
    | Ok
    /// We didn't run the test, because it's [<Ignore>].
    | Ignored of reason : string option
    /// We didn't run the test, because it's [<Explicit>].
    | Explicit of reason : string option

/// Represents the failure of a test.
[<RequireQualifiedAccess>]
type TestMemberFailure =
    /// We couldn't run this test because it was somehow malformed in a way we detected up front.
    | Malformed of reasons : string list
    /// We tried to run the test, but it failed. (A single test can fail many times, e.g. if it failed and also
    /// the tear-down logic failed afterwards.)
    | Failed of TestFailure list

type FixtureRunResults =
    {
        Failed : TestMemberFailure list
        SuccessCount : int
        OtherFailures : UserMethodFailure list
    }

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
        (test : MethodInfo)
        (containingObject : obj)
        (args : obj[])
        : Result<unit, TestFailure list>
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
                    with e ->
                        Error (UserMethodFailure.Threw (head.Name, e))

                match result with
                | Error e -> Error (wrap e)
                | Ok result ->
                    match result with
                    | :? unit -> runMethods wrap rest args
                    | ret -> UserMethodFailure.ReturnedNonUnit (head.Name, ret) |> wrap |> Error

        match runMethods TestFailure.SetUpFailed setUp [||] with
        | Error e -> Error [ e ]
        | Ok () ->

        let result = runMethods TestFailure.TestFailed [ test ] args

        // Unconditionally run TearDown after tests, even if tests failed.
        let tearDownResult = runMethods TestFailure.TearDownFailed tearDown [||]

        match result, tearDownResult with
        | Ok (), Ok () -> Ok ()
        | Error e, Ok ()
        | Ok (), Error e -> Error [ e ]
        | Error e1, Error e2 -> Error [ e1 ; e2 ]

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
                    $"Test %s{test.Name} has a parameter with the Values attribute and a parameter without. All parameters must have Values if any one does."

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
        : Result<TestMemberSuccess, TestMemberFailure> list
        =
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
        | Some result -> [ Ok result ]
        | None ->

        Seq.init
            (Option.defaultValue 1 test.Repeat)
            (fun _ ->
                let values = getValues test

                match values with
                | Error e -> Seq.singleton (Error e)
                | Ok values ->

                let inline normaliseError
                    (e : Result<unit, TestFailure list>)
                    : Result<TestMemberSuccess, TestMemberFailure>
                    =
                    match e with
                    | Ok () -> Ok TestMemberSuccess.Ok
                    | Error e -> Error (e |> TestMemberFailure.Failed)

                match test.Kind, values with
                | TestKind.Data data, None ->
                    data
                    |> Seq.map (fun args ->
                        runOne setUp tearDown test.Method containingObject (Array.ofList args)
                        |> normaliseError
                    )
                | TestKind.Data _, Some _ ->
                    [
                        "Test has both the TestCase and Values attributes. Specify one or the other."
                    ]
                    |> TestMemberFailure.Malformed
                    |> Error
                    |> Seq.singleton
                | TestKind.Single, None ->
                    runOne setUp tearDown test.Method containingObject [||]
                    |> normaliseError
                    |> Seq.singleton
                | TestKind.Single, Some vals ->
                    let combinatorial =
                        Option.defaultValue Combinatorial.Combinatorial test.Combinatorial

                    match combinatorial with
                    | Combinatorial.Combinatorial ->
                        vals
                        |> Seq.map (fun l -> l |> Seq.map (fun v -> v.Value) |> Seq.toList)
                        |> Seq.toList
                        |> List.combinations
                        |> Seq.map (fun args ->
                            runOne setUp tearDown test.Method containingObject (Array.ofList args)
                            |> normaliseError
                        )
                    | Combinatorial.Sequential ->
                        let maxLength = vals |> Seq.map (fun i -> i.Count) |> Seq.max

                        seq {
                            for i = 0 to maxLength - 1 do
                                let args =
                                    vals
                                    |> Array.map (fun param -> if i >= param.Count then null else param.[i].Value)

                                yield runOne setUp tearDown test.Method containingObject args |> normaliseError
                        }
                | TestKind.Source _, Some _ ->
                    [
                        "Test has both the TestCaseSource and Values attributes. Specify one or the other."
                    ]
                    |> TestMemberFailure.Malformed
                    |> Error
                    |> Seq.singleton
                | TestKind.Source sources, None ->
                    seq {
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
                                    match arg with
                                    | :? Tuple<obj, obj> as (a, b) ->
                                        runOne setUp tearDown test.Method containingObject [| a ; b |]
                                    | :? Tuple<obj, obj, obj> as (a, b, c) ->
                                        runOne setUp tearDown test.Method containingObject [| a ; b ; c |]
                                    | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) ->
                                        runOne setUp tearDown test.Method containingObject [| a ; b ; c ; d |]
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

                                            runOne
                                                setUp
                                                tearDown
                                                test.Method
                                                containingObject
                                                (argsMem.Invoke (arg, [||]) |> unbox<obj[]>)
                                        else
                                            runOne setUp tearDown test.Method containingObject [| arg |]
                                    |> normaliseError
                    }
            )
        |> Seq.concat
        |> Seq.toList

    /// Run every test (except those which fail the `filter`) in this test fixture, as well as the
    /// appropriate setup and tear-down logic.
    let run (filter : TestFixture -> SingleTestMethod -> bool) (tests : TestFixture) : FixtureRunResults =
        eprintfn $"Running test fixture: %s{tests.Name} (%i{tests.Tests.Length} tests to run)"

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

        let setupResult =
            match tests.OneTimeSetUp with
            | Some su ->
                try
                    match su.Invoke (containingObject, [||]) with
                    | :? unit -> None
                    | ret -> Some (UserMethodFailure.ReturnedNonUnit (su.Name, ret))
                with e ->
                    Some (UserMethodFailure.Threw (su.Name, e))
            | _ -> None

        let totalTestSuccess = ref 0
        let testFailures = ResizeArray ()

        match setupResult with
        | Some _ ->
            // Don't run any tests if setup failed.
            ()
        | None ->
            for test in tests.Tests do
                if filter tests test then
                    eprintfn $"Running test: %s{test.Name}"
                    let testSuccess = ref 0

                    let results = runTestsFromMember tests.SetUp tests.TearDown containingObject test

                    for result in results do
                        match result with
                        | Error failure ->
                            testFailures.Add failure
                            eprintfn $"Test failed: %O{failure}"
                        | Ok _ -> Interlocked.Increment testSuccess |> ignore<int>

                    Interlocked.Add (totalTestSuccess, testSuccess.Value) |> ignore<int>
                    eprintfn $"Finished test %s{test.Name} (%i{testSuccess.Value} success)"
                else
                    eprintfn $"Skipping test due to filter: %s{test.Name}"

        // Unconditionally run OneTimeTearDown if it exists.
        let tearDownError =
            match tests.OneTimeTearDown with
            | Some td ->
                try
                    match td.Invoke (containingObject, [||]) with
                    | null -> None
                    | ret -> Some (UserMethodFailure.ReturnedNonUnit (td.Name, ret))
                with :? TargetInvocationException as e ->
                    Some (UserMethodFailure.Threw (td.Name, e))
            | _ -> None

        {
            Failed = testFailures |> Seq.toList
            SuccessCount = totalTestSuccess.Value
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

        (TestFixture.Empty parentType.Name, parentType.GetRuntimeMethods ())
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
