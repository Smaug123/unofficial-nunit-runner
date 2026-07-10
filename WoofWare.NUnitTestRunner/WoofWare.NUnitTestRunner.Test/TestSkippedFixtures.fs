namespace WoofWare.NUnitTestRunner.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.NUnitTestRunner

/// A module deliberately carrying no NUnit attributes: NUnit must not discover it, because every test in it
/// fails. Instead, TestSkippedFixtures drives it through TestFixture.run.
module SkippedFixture =
    /// Lets us name this module's compiled Type via typeof.
    type Marker = class end

    let firstTest () : unit =
        failwith "should not run: fixture is skipped"

    let secondTest () : unit =
        failwith "should not run: fixture is skipped"

    let sourceCases : int list = [ 1 ; 2 ]

    let dataTest (i : int) : unit =
        ignore i
        failwith "should not run: fixture is skipped"

    let sourceTest (i : int) : unit =
        ignore i
        failwith "should not run: fixture is skipped"

    let valuesTest ([<Values(true, false)>] b : bool) : unit =
        ignore b
        failwith "should not run: fixture is skipped"

[<TestFixture>]
module TestSkippedFixtures =

    let private noOpProgress =
        { new ITestProgress with
            member _.OnTestFixtureStart _ _ = ()
            member _.OnTestFixtureSkipped _ _ = ()
            member _.OnTestMemberStart _ = ()
            member _.OnTestFailed _ _ = ()
            member _.OnTestMemberFinished _ = ()
            member _.OnTestMemberSkipped _ = ()
        }

    let private makeTestOfKind (kind : TestKind) (name : string) : SingleTestMethod =
        {
            Method = typeof<SkippedFixture.Marker>.DeclaringType.GetMethod name
            Kind = kind
            Modifiers = []
            Categories = []
            Repeat = None
            Combinatorial = None
            Parallelize = None
        }

    let private makeFixture (modifier : Modifier) : TestFixture =
        let moduleType = typeof<SkippedFixture.Marker>.DeclaringType

        let makeTest (name : string) : SingleTestMethod = makeTestOfKind TestKind.Single name

        { TestFixture.Empty moduleType None [ modifier ] [] with
            Tests =
                [
                    makeTest (nameof SkippedFixture.firstTest)
                    makeTest (nameof SkippedFixture.secondTest)
                ]
        }

    let skippedFixtureCases =
        [
            Modifier.Explicit (Some "too slow"), TestMemberSuccess.Explicit (Some "too slow")
            Modifier.Explicit None, TestMemberSuccess.Explicit None
            Modifier.Ignored (Some "broken"), TestMemberSuccess.Ignored (Some "broken")
            Modifier.Ignored None, TestMemberSuccess.Ignored None
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof skippedFixtureCases)>]
    let ``A skipped fixture reports each of its tests as not executed``
        (modifier : Modifier, expected : TestMemberSuccess)
        =
        let fixture = makeFixture modifier

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let results =
            (TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture).Result

        let results = results |> List.exactlyOne
        results.Failed |> shouldBeEmpty
        results.OtherFailures |> shouldBeEmpty

        results.Success
        |> List.map (fun (test, result, metadata) ->
            metadata.TestName |> shouldEqual test.Name
            test.Name, result
        )
        |> List.sortBy fst
        |> shouldEqual
            [
                nameof SkippedFixture.firstTest, expected
                nameof SkippedFixture.secondTest, expected
            ]

    [<Test>]
    let ``A skipped fixture only reports tests which pass the filter`` () =
        let fixture = makeFixture (Modifier.Ignored (Some "broken"))

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let skipped = ResizeArray ()

        let progress =
            { new ITestProgress with
                member _.OnTestFixtureStart _ _ = ()
                member _.OnTestFixtureSkipped _ _ = ()
                member _.OnTestMemberStart _ = ()
                member _.OnTestFailed _ _ = ()
                member _.OnTestMemberFinished _ = ()

                member _.OnTestMemberSkipped name =
                    lock skipped (fun () -> skipped.Add name)
            }

        let filter (_ : TestFixture) (test : SingleTestMethod) =
            test.Name = nameof SkippedFixture.firstTest

        let results = (TestFixture.run contexts par progress filter fixture).Result

        let results = results |> List.exactlyOne
        results.Failed |> shouldBeEmpty
        results.OtherFailures |> shouldBeEmpty

        results.Success
        |> List.map (fun (test, result, _) -> test.Name, result)
        |> shouldEqual [ nameof SkippedFixture.firstTest, TestMemberSuccess.Ignored (Some "broken") ]

        skipped |> Seq.toList |> shouldEqual [ nameof SkippedFixture.secondTest ]

    [<Test>]
    let ``A skipped fixture reports each individual test case of a data-driven test`` () =
        let moduleType = typeof<SkippedFixture.Marker>.DeclaringType

        let fixture =
            { TestFixture.Empty moduleType None [ Modifier.Ignored (Some "broken") ] [] with
                Tests =
                    [
                        makeTestOfKind TestKind.Single (nameof SkippedFixture.firstTest)
                        makeTestOfKind (TestKind.Data [ [ box 1 ] ; [ box 2 ] ]) (nameof SkippedFixture.dataTest)
                        makeTestOfKind
                            (TestKind.Source [ nameof SkippedFixture.sourceCases ])
                            (nameof SkippedFixture.sourceTest)
                        makeTestOfKind TestKind.Single (nameof SkippedFixture.valuesTest)
                    ]
            }

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let results =
            (TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture).Result

        let results = results |> List.exactlyOne
        results.Failed |> shouldBeEmpty
        results.OtherFailures |> shouldBeEmpty

        for _, result, _ in results.Success do
            result |> shouldEqual (TestMemberSuccess.Ignored (Some "broken"))

        results.Success
        |> List.map (fun (_, _, metadata) -> metadata.TestName)
        |> List.sort
        |> shouldEqual
            [
                "dataTest(1)"
                "dataTest(2)"
                "firstTest"
                "sourceTest(1)"
                "sourceTest(2)"
                "valuesTest(False)"
                "valuesTest(True)"
            ]

    [<Test>]
    let ``An individually ignored data-driven test reports each of its cases`` () =
        let moduleType = typeof<SkippedFixture.Marker>.DeclaringType

        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                Tests =
                    [
                        { makeTestOfKind (TestKind.Data [ [ box 1 ] ; [ box 2 ] ]) (nameof SkippedFixture.dataTest) with
                            Modifiers = [ Modifier.Ignored (Some "broken") ]
                        }
                    ]
            }

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let results =
            (TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture).Result

        let results = results |> List.exactlyOne
        results.Failed |> shouldBeEmpty
        results.OtherFailures |> shouldBeEmpty

        results.Success
        |> List.map (fun (_, result, metadata) -> metadata.TestName, result)
        |> List.sortBy fst
        |> shouldEqual
            [
                "dataTest(1)", TestMemberSuccess.Ignored (Some "broken")
                "dataTest(2)", TestMemberSuccess.Ignored (Some "broken")
            ]

    [<Test>]
    let ``A skipped parameterised fixture reports each of its tests once per parameter set`` () =
        let fixture =
            { makeFixture (Modifier.Explicit None) with
                Parameters = [ [ box 1 ] ; [ box 2 ] ]
            }

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let results =
            (TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture).Result

        results |> shouldHaveLength 2

        for result in results do
            result.Failed |> shouldBeEmpty
            result.OtherFailures |> shouldBeEmpty

            result.Success
            |> List.map (fun (test, result, _) -> test.Name, result)
            |> List.sortBy fst
            |> shouldEqual
                [
                    nameof SkippedFixture.firstTest, TestMemberSuccess.Explicit None
                    nameof SkippedFixture.secondTest, TestMemberSuccess.Explicit None
                ]
