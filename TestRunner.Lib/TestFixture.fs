namespace TestRunner

open System
open System.Reflection
open System.Threading
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TestFixture =
    let private runOne
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (test : MethodInfo)
        (args : obj[])
        : Result<unit, TestFailure>
        =
        try
            for setup in setUp do
                if not (isNull (setup.Invoke (null, [||]))) then
                    failwith $"Setup procedure '%s{setup.Name}' returned non-null"

            try
                match test.Invoke (null, args) with
                | :? unit -> Ok ()
                | ret -> Error (TestReturnedNonUnit ret)
            with exc ->
                Error (TestThrew exc.InnerException)

        finally
            for tearDown in tearDown do
                if not (isNull (tearDown.Invoke (null, [||]))) then
                    failwith $"Teardown procedure '%s{tearDown.Name}' returned non-null"

    let private runFixture
        (setUp : MethodInfo list)
        (tearDown : MethodInfo list)
        (test : SingleTestMethod)
        : Result<unit, TestFailure> list
        =
        let shouldRunTest =
            (true, test.Modifiers)
            ||> List.fold (fun _ modifier ->
                match modifier with
                | Modifier.Explicit reason ->
                    // TODO: if the filter explicitly says to run this, then do so
                    let reason =
                        match reason with
                        | None -> ""
                        | Some r -> $" (%s{r})"

                    printfn $"Will ignore test %s{test.Name} because it is marked explicit%s{reason}"
                    false
                | Modifier.Ignored reason ->
                    let reason =
                        match reason with
                        | None -> ""
                        | Some r -> $" (%s{r})"

                    eprintfn $"Will ignore test %s{test.Name} because it is marked ignored%s{reason}"
                    false
            )

        if not shouldRunTest then
            []
        else

        Seq.init
            (Option.defaultValue 1 test.Repeat)
            (fun _ ->
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
                            | [] -> None
                            | [ x ] -> Some x
                            | _ :: _ :: _ ->
                                failwith
                                    $"Test %s{test.Name} has multiple Values attributes on a parameter. Exactly one per parameter please."
                    )

                let valuesAttrs =
                    if valuesAttrs |> Array.exists (fun l -> l.IsSome) then
                        if valuesAttrs |> Array.exists (fun l -> l.IsNone) then
                            failwith
                                $"Test %s{test.Name} has a parameter with the Values attribute and a parameter without. All parameters must have Values if any one does."

                        Choice1Of2 (valuesAttrs |> Array.map Option.get)
                    else
                        Choice2Of2 ()

                match test.Kind, valuesAttrs with
                | TestKind.Data data, Choice2Of2 () ->
                    data
                    |> Seq.map (fun args -> runOne setUp tearDown test.Method (Array.ofList args))
                | TestKind.Data _, Choice1Of2 _ ->
                    failwith
                        $"Test %s{test.Name} has both the TestCase and Values attributes. Specify one or the other."
                | TestKind.Single, Choice2Of2 () -> Seq.singleton (runOne setUp tearDown test.Method [||])
                | TestKind.Single, Choice1Of2 vals ->
                    let combinatorial =
                        Option.defaultValue Combinatorial.Combinatorial test.Combinatorial

                    match combinatorial with
                    | Combinatorial.Combinatorial ->
                        vals
                        |> Seq.map (fun l -> l |> Seq.map (fun v -> v.Value) |> Seq.toList)
                        |> Seq.toList
                        |> List.combinations
                        |> Seq.map (fun args -> runOne setUp tearDown test.Method (Array.ofList args))
                    | Combinatorial.Sequential ->
                        let maxLength = vals |> Seq.map (fun i -> i.Count) |> Seq.max

                        seq {
                            for i = 0 to maxLength - 1 do
                                let args =
                                    vals
                                    |> Array.map (fun param -> if i >= param.Count then null else param.[i].Value)

                                runOne setUp tearDown test.Method args
                        }
                | TestKind.Source _, Choice1Of2 _ ->
                    failwith
                        $"Test %s{test.Name} has both the TestCaseSource and Values attributes. Specify one or the other."
                | TestKind.Source s, Choice2Of2 () ->
                    let args =
                        test.Method.DeclaringType.GetProperty (
                            s,
                            BindingFlags.Public
                            ||| BindingFlags.NonPublic
                            ||| BindingFlags.Instance
                            ||| BindingFlags.Static
                        )

                    seq {
                        // Might not be an IEnumerable of a reference type.
                        // Concretely, `FSharpList<HttpStatusCode> :> IEnumerable<obj>` fails.
                        for arg in args.GetValue null :?> System.Collections.IEnumerable do
                            yield
                                match arg with
                                | :? Tuple<obj, obj> as (a, b) -> runOne setUp tearDown test.Method [| a ; b |]
                                | :? Tuple<obj, obj, obj> as (a, b, c) ->
                                    runOne setUp tearDown test.Method [| a ; b ; c |]
                                | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) ->
                                    runOne setUp tearDown test.Method [| a ; b ; c ; d |]
                                | arg ->
                                    let argTy = arg.GetType ()

                                    if argTy.FullName = "NUnit.TestCaseData" then
                                        let field =
                                            argTy.GetField ("Arguments", BindingFlags.Public ||| BindingFlags.Instance)

                                        if isNull field then
                                            failwith "Unexpectedly could not call `.Arguments` on TestCaseData"

                                        runOne setUp tearDown test.Method (field.GetValue () |> unbox<obj[]>)
                                    else
                                        runOne setUp tearDown test.Method [| arg |]
                    }
            )
        |> Seq.concat
        |> Seq.toList

    let rec shouldRun (filter : Filter) : TestFixture -> SingleTestMethod -> bool =
        match filter with
        | Filter.Not filter ->
            let inner = shouldRun filter
            fun a b -> not (inner a b)
        | Filter.And (a, b) ->
            let inner1 = shouldRun a
            let inner2 = shouldRun b
            fun a b -> inner1 a b && inner2 a b
        | Filter.Or (a, b) ->
            let inner1 = shouldRun a
            let inner2 = shouldRun b
            fun a b -> inner1 a b || inner2 a b
        | Filter.Name (Match.Exact m) -> fun _fixture method -> method.Method.Name = m
        | Filter.Name (Match.Contains m) -> fun _fixture method -> method.Method.Name.Contains m
        | Filter.FullyQualifiedName (Match.Exact m) -> fun fixture method -> (fixture.Name + method.Method.Name) = m
        | Filter.FullyQualifiedName (Match.Contains m) ->
            fun fixture method -> (fixture.Name + method.Method.Name).Contains m
        | Filter.TestCategory (Match.Contains m) ->
            fun _fixture method -> method.Categories |> List.exists (fun cat -> cat.Contains m)
        | Filter.TestCategory (Match.Exact m) -> fun _fixture method -> method.Categories |> List.contains m

    let run (filter : TestFixture -> SingleTestMethod -> bool) (tests : TestFixture) : int =
        eprintfn $"Running test fixture: %s{tests.Name} (%i{tests.Tests.Length} tests to run)"

        match tests.OneTimeSetUp with
        | Some su ->
            if not (isNull (su.Invoke (null, [||]))) then
                failwith $"One-time setup procedure '%s{su.Name}' returned non-null"
        | _ -> ()

        let totalTestSuccess = ref 0
        let testFailures = ref 0

        try
            for test in tests.Tests do
                if filter tests test then
                    eprintfn $"Running test: %s{test.Name}"
                    let testSuccess = ref 0

                    let results = runFixture tests.SetUp tests.TearDown test

                    for result in results do
                        match result with
                        | Error exc ->
                            eprintfn $"Test failed: %O{exc}"
                            Interlocked.Increment testFailures |> ignore<int>
                        | Ok () -> Interlocked.Increment testSuccess |> ignore<int>

                    Interlocked.Add (totalTestSuccess, testSuccess.Value) |> ignore<int>
                    eprintfn $"Finished test %s{test.Name} (%i{testSuccess.Value} success)"
                else
                    eprintfn $"Skipping test due to filter: %s{test.Name}"
        finally
            match tests.OneTimeTearDown with
            | Some td ->
                if not (isNull (td.Invoke (null, [||]))) then
                    failwith $"TearDown procedure '%s{td.Name}' returned non-null"
            | _ -> ()

        eprintfn $"Test fixture %s{tests.Name} completed (%i{totalTestSuccess.Value} success)."
        testFailures.Value

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
