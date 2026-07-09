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

    let private makeFixture (modifier : Modifier) : TestFixture =
        let moduleType = typeof<SkippedFixture.Marker>.DeclaringType

        let makeTest (name : string) : SingleTestMethod =
            {
                Method = moduleType.GetMethod name
                Kind = TestKind.Single
                Modifiers = []
                Categories = []
                Repeat = None
                Combinatorial = None
                Parallelize = None
            }

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
