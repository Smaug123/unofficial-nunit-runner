namespace TestRunner

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open NUnit.Framework

type Modifier =
    | Explicit of reason : string option
    | Ignored of reason : string option

type TestKind =
    | Single
    | Source of string
    | Data of obj list list

type SingleTestMethod =
    {
        // TODO: cope with [<Values>] on the parameters
        Method : MethodInfo
        Kind : TestKind
        Modifiers : Modifier list
        Categories : string list
        Repeat : int option
    }

    member this.Name = this.Method.Name

[<RequireQualifiedAccess>]
module SingleTestMethod =
    let parse (parentCategories : string list) (method : MethodInfo) : SingleTestMethod option =
        let isTest, hasSource, hasData, modifiers, categories, repeat =
            ((false, None, None, [], [], None), method.CustomAttributes)
            ||> Seq.fold (fun (isTest, hasSource, hasData, mods, cats, repeat) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.TestAttribute" ->
                    if attr.ConstructorArguments.Count > 0 then
                        failwith "Unexpectedly got arguments to the Test attribute"

                    (true, hasSource, hasData, mods, cats, repeat)
                | "NUnit.Framework.TestCaseAttribute" ->
                    let args = attr.ConstructorArguments |> Seq.map _.Value |> Seq.toList

                    match hasData with
                    | None -> (isTest, hasSource, Some [ List.ofSeq args ], mods, cats, repeat)
                    | Some existing -> (isTest, hasSource, Some ((List.ofSeq args) :: existing), mods, cats, repeat)
                | "NUnit.Framework.TestCaseSourceAttribute" ->
                    let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    match hasSource with
                    | None -> (isTest, Some arg, hasData, mods, cats, repeat)
                    | Some existing ->
                        failwith
                            $"Unexpectedly got multiple different sources for test %s{method.Name} (%s{existing}, %s{arg})"
                | "NUnit.Framework.ExplicitAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (isTest, hasSource, hasData, (Modifier.Explicit reason) :: mods, cats, repeat)
                | "NUnit.Framework.IgnoreAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (isTest, hasSource, hasData, (Modifier.Ignored reason) :: mods, cats, repeat)
                | "NUnit.Framework.CategoryAttribute" ->
                    let category =
                        attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    (isTest, hasSource, hasData, mods, category :: cats, repeat)
                | "NUnit.Framework.RepeatAttribute" ->
                    match repeat with
                    | Some _ -> failwith $"Got RepeatAttribute multiple times on %s{method.Name}"
                    | None ->

                    let repeat = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<int>
                    (isTest, hasSource, hasData, mods, cats, Some repeat)
                | s when s.StartsWith ("NUnit.Framework", StringComparison.Ordinal) ->
                    failwith $"Unrecognised attribute on function %s{method.Name}: %s{attr.AttributeType.FullName}"
                | _ -> (isTest, hasSource, hasData, mods, cats, repeat)
            )

        match isTest, hasSource, hasData, modifiers, categories, repeat with
        | _, Some _, Some _, _, _, _ ->
            failwith $"Test %s{method.Name} unexpectedly has both TestData and TestCaseSource; not currently supported"
        | false, None, None, [], _, _ -> None
        | _, Some source, None, mods, categories, repeat ->
            {
                Kind = TestKind.Source source
                Method = method
                Modifiers = mods
                Categories = categories @ parentCategories
                Repeat = repeat
            }
            |> Some
        | _, None, Some data, mods, categories, repeat ->
            {
                Kind = TestKind.Data data
                Method = method
                Modifiers = mods
                Categories = categories @ parentCategories
                Repeat = repeat
            }
            |> Some
        | true, None, None, mods, categories, repeat ->
            {
                Kind = TestKind.Single
                Method = method
                Modifiers = mods
                Categories = categories @ parentCategories
                Repeat = repeat
            }
            |> Some
        | false, None, None, _ :: _, _, _ ->
            failwith
                $"Unexpectedly got test modifiers but no test settings on '%s{method.Name}', which you probably didn't intend."

type TestFixture =
    {
        Name : string
        OneTimeSetUp : MethodInfo option
        OneTimeTearDown : MethodInfo option
        Tests : SingleTestMethod list
    }

    static member Empty (name : string) =
        {
            Name = name
            OneTimeSetUp = None
            OneTimeTearDown = None
            Tests = []
        }

type TestFailure =
    | TestReturnedNonUnit of obj
    | TestThrew of exn

    override this.ToString () =
        match this with
        | TestFailure.TestReturnedNonUnit ret -> $"Test returned a non-unit: %O{ret}"
        | TestFailure.TestThrew exc -> $"Test threw: %s{exc.Message}\n  %s{exc.StackTrace}"

[<RequireQualifiedAccess>]
module TestFixture =
    let private runOne (test : MethodInfo) (args : obj[]) : Result<unit, TestFailure> =
        try
            match test.Invoke (null, args) with
            | :? unit -> Ok ()
            | ret -> Error (TestReturnedNonUnit ret)
        with exc ->
            Error (TestThrew exc)

    let private runFixture (test : SingleTestMethod) : Result<unit, TestFailure> list =
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
                match test.Kind with
                | TestKind.Data data -> data |> Seq.map (fun args -> runOne test.Method (Array.ofList args))
                | TestKind.Single -> Seq.singleton (runOne test.Method [||])
                | TestKind.Source s ->
                    let args =
                        test.Method.DeclaringType.GetProperty (
                            s,
                            BindingFlags.Public
                            ||| BindingFlags.NonPublic
                            ||| BindingFlags.Instance
                            ||| BindingFlags.Static
                        )

                    args.GetValue null :?> IEnumerable<obj>
                    |> Seq.map (fun arg ->
                        match arg with
                        | :? TestCaseData as tcd -> runOne test.Method tcd.Arguments
                        | :? Tuple<obj, obj> as (a, b) -> runOne test.Method [| a ; b |]
                        | :? Tuple<obj, obj, obj> as (a, b, c) -> runOne test.Method [| a ; b ; c |]
                        | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) -> runOne test.Method [| a ; b ; c ; d |]
                        | arg -> runOne test.Method [| arg |]
                    )
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
                failwith "Setup procedure returned non-null"
        | _ -> ()

        let totalTestSuccess = ref 0
        let testFailures = ref 0

        try
            for test in tests.Tests do
                if filter tests test then
                    eprintfn $"Running test: %s{test.Name}"
                    let testSuccess = ref 0

                    let results = runFixture test

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
                    failwith "TearDown procedure returned non-null"
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
            if
                mi.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.OneTimeSetUpAttribute")
            then
                match state.OneTimeSetUp with
                | None ->
                    { state with
                        OneTimeSetUp = Some mi
                    }
                | Some _existing -> failwith "Multiple OneTimeSetUp methods found"
            elif
                mi.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.OneTimeTearDownAttribute")
            then
                match state.OneTimeTearDown with
                | None ->
                    { state with
                        OneTimeTearDown = Some mi
                    }
                | Some _existing -> failwith "Multiple OneTimeTearDown methods found"
            else
                match SingleTestMethod.parse categories mi with
                | Some test ->
                    { state with
                        Tests = test :: state.Tests
                    }
                | None -> state
        )

module Program =
    [<EntryPoint>]
    let main argv =
        let testDll, filter =
            match argv |> List.ofSeq with
            | [ dll ] -> FileInfo dll, None
            | [ dll ; "--filter" ; filter ] -> FileInfo dll, Some (FilterIntermediate.parse filter |> Filter.make)
            | _ -> failwith "provide exactly one arg, a test DLL"

        let filter =
            match filter with
            | Some filter -> TestFixture.shouldRun filter
            | None -> fun _ _ -> true

        let assy = Assembly.LoadFrom testDll.FullName

        assy.ExportedTypes
        // TODO: NUnit nowadays doesn't care if you're a TestFixture or not
        |> Seq.filter (fun ty ->
            ty.CustomAttributes
            |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TestFixtureAttribute")
        )
        |> Seq.iter (fun ty ->
            let testFixture = TestFixture.parse ty

            match TestFixture.run filter testFixture with
            | 0 -> ()
            | i -> eprintfn $"%i{i} tests failed"
        )

        0
