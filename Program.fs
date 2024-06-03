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
        Method : MethodInfo
        Kind : TestKind
        Modifiers : Modifier list
    }

    member this.Name = this.Method.Name

[<RequireQualifiedAccess>]
module SingleTestMethod =
    let parse (method : MethodInfo) : SingleTestMethod option =
        let isTest, hasSource, hasData, modifiers =
            ((false, None, None, []), method.CustomAttributes)
            ||> Seq.fold (fun (isTest, hasSource, hasData, mods) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.TestAttribute" ->
                    if attr.ConstructorArguments.Count > 0 then
                        failwith "Unexpectedly got arguments to the Test attribute"

                    (true, hasSource, hasData, mods)
                | "NUnit.Framework.TestCaseAttribute" ->
                    let args = attr.ConstructorArguments |> Seq.map _.Value |> Seq.toList

                    match hasData with
                    | None -> (isTest, hasSource, Some [ List.ofSeq args ], mods)
                    | Some existing -> (isTest, hasSource, Some ((List.ofSeq args) :: existing), mods)
                | "NUnit.Framework.TestCaseSourceAttribute" ->
                    let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    match hasSource with
                    | None -> (isTest, Some arg, hasData, mods)
                    | Some existing ->
                        failwith
                            $"Unexpectedly got multiple different sources for test %s{method.Name} (%s{existing}, %s{arg})"
                | "NUnit.Framework.ExplicitAttribute" ->
                    let reason = attr.ConstructorArguments |> Seq.tryHead |> Option.map unbox<string>
                    (isTest, hasSource, hasData, (Modifier.Explicit reason) :: mods)
                | "NUnit.Framework.IgnoreAttribute" ->
                    let reason = attr.ConstructorArguments |> Seq.tryHead |> Option.map unbox<string>
                    (isTest, hasSource, hasData, (Modifier.Ignored reason) :: mods)
                | s when s.StartsWith ("NUnit.Framework", StringComparison.Ordinal) ->
                    failwith $"Unrecognised attribute on function %s{method.Name}: %s{attr.AttributeType.FullName}"
                | _ -> (isTest, hasSource, hasData, mods)
            )

        match isTest, hasSource, hasData, modifiers with
        | _, Some _, Some _, _ ->
            failwith $"Test %s{method.Name} unexpectedly has both TestData and TestCaseSource; not currently supported"
        | false, None, None, [] -> None
        | _, Some source, None, mods ->
            {
                Kind = TestKind.Source source
                Method = method
                Modifiers = mods
            }
            |> Some
        | _, None, Some data, mods ->
            {
                Kind = TestKind.Data data
                Method = method
                Modifiers = mods
            }
            |> Some
        | true, None, None, mods ->
            {
                Kind = TestKind.Single
                Method = method
                Modifiers = mods
            }
            |> Some
        | false, None, None, mods ->
            failwith
                $"Unexpectedly got test modifiers but no test settings on '%s{method.Name}', which you probably didn't intend."

type TestFixture =
    {
        Name : string
        SetUp : MethodInfo option
        TearDown : MethodInfo option
        Tests : SingleTestMethod list
    }

    static member Empty (name : string) =
        {
            Name = name
            SetUp = None
            TearDown = None
            Tests = []
        }

type TestFailure =
    | TestReturnedNonUnit of obj
    | TestThrew of exn

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

        match test.Kind with
        | TestKind.Data data -> data |> List.map (fun args -> runOne test.Method (Array.ofList args))
        | TestKind.Single -> [ runOne test.Method [||] ]
        | TestKind.Source s ->
            let args = test.Method.DeclaringType.GetProperty s

            args.GetValue null :?> IEnumerable<obj>
            |> Seq.map (fun arg ->
                match arg with
                | :? TestCaseData as tcd -> runOne test.Method tcd.Arguments
                | :? Tuple<obj, obj> as (a, b) -> runOne test.Method [| a ; b |]
                | :? Tuple<obj, obj, obj> as (a, b, c) -> runOne test.Method [| a ; b ; c |]
                | :? Tuple<obj, obj, obj, obj> as (a, b, c, d) -> runOne test.Method [| a ; b ; c ; d |]
                | arg -> runOne test.Method [| arg |]
            )
            |> List.ofSeq

    let run (tests : TestFixture) : int =
        eprintfn $"Running test fixture: %s{tests.Name} (%i{tests.Tests.Length} tests to run)"

        match tests.SetUp with
        | Some su ->
            if not (isNull (su.Invoke (null, [||]))) then
                failwith "Setup procedure returned non-null"
        | _ -> ()

        let totalTestSuccess = ref 0
        let testFailures = ref 0

        try
            for test in tests.Tests do
                eprintfn $"Running test: %s{test.Name}"
                let testSuccess = ref 0

                let results = runFixture test

                for result in results do
                    match result with
                    | Error exc ->
                        eprintfn $"Test failed: {exc}"
                        Interlocked.Increment testFailures |> ignore<int>
                    | Ok () -> Interlocked.Increment testSuccess |> ignore<int>

                Interlocked.Add (totalTestSuccess, testSuccess.Value) |> ignore<int>
                eprintfn $"Finished test %s{test.Name} (%i{testSuccess.Value} success)"
        finally
            match tests.TearDown with
            | Some td ->
                if not (isNull (td.Invoke (null, [||]))) then
                    failwith "TearDown procedure returned non-null"
            | _ -> ()

        eprintfn $"Test fixture %s{tests.Name} completed (%i{totalTestSuccess.Value} success)."
        testFailures.Value

    let parse (parentType : Type) : TestFixture =
        (TestFixture.Empty parentType.Name, parentType.GetRuntimeMethods ())
        ||> Seq.fold (fun state mi ->
            if
                mi.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.SetUpAttribute")
            then
                match state.SetUp with
                | None ->
                    { state with
                        SetUp = Some mi
                    }
                | Some _existing -> failwith "Multiple SetUp methods found"
            elif
                mi.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TearDownAttribute")
            then
                match state.TearDown with
                | None ->
                    { state with
                        TearDown = Some mi
                    }
                | Some _existing -> failwith "Multiple TearDown methods found"
            else
                match SingleTestMethod.parse mi with
                | Some test ->
                    { state with
                        Tests = test :: state.Tests
                    }
                | None -> state
        )

module Program =
    [<EntryPoint>]
    let main argv =
        let testDll =
            match argv with
            | [| dll |] -> FileInfo dll
            | _ -> failwith "provide exactly one arg, a test DLL"

        let assy = Assembly.LoadFrom testDll.FullName

        assy.ExportedTypes
        |> Seq.filter (fun ty ->
            ty.CustomAttributes
            |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TestFixtureAttribute")
        )
        |> Seq.iter (fun ty ->
            let testFixture = TestFixture.parse ty

            match TestFixture.run testFixture with
            | 0 -> ()
            | i -> eprintfn $"%i{i} tests failed"
        )

        0
