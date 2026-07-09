namespace WoofWare.NUnitTestRunner.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.NUnitTestRunner

/// A module deliberately carrying no NUnit attributes: NUnit must not discover it, because every test in it
/// fails. Instead, TestFixtureRunner drives it through TestFixture.run.
module ManyFailuresFixture =
    /// Lets us name this module's compiled Type via typeof.
    type Marker = class end

    let cases : int list = List.init 1000 id

    let alwaysFails (i : int) : unit =
        ignore i
        failwith "deliberate failure"

[<TestFixture>]
module TestFixtureRunner =

    let private noOpProgress =
        { new ITestProgress with
            member _.OnTestFixtureStart _ _ = ()
            member _.OnTestFixtureSkipped _ _ = ()
            member _.OnTestMemberStart _ = ()
            member _.OnTestFailed _ _ = ()
            member _.OnTestMemberFinished _ = ()
            member _.OnTestMemberSkipped _ = ()
        }

    [<Test>]
    let ``Failures reported concurrently are all retained`` () =
        let moduleType = typeof<ManyFailuresFixture.Marker>.DeclaringType

        let testMethod = moduleType.GetMethod (nameof ManyFailuresFixture.alwaysFails)

        let test : SingleTestMethod =
            {
                Method = testMethod
                Kind = TestKind.Source [ nameof ManyFailuresFixture.cases ]
                Modifiers = []
                Categories = []
                Repeat = None
                Combinatorial = None
                Parallelize = None
            }

        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                Tests = [ test ]
            }

        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (Some 16, None)

        let results =
            (TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture).Result

        let results = results |> List.exactlyOne
        results.OtherFailures |> shouldBeEmpty
        results.Success |> shouldBeEmpty

        results.Failed |> List.length |> shouldEqual ManyFailuresFixture.cases.Length
