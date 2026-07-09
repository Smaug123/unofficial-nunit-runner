namespace WoofWare.NUnitTestRunner.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open WoofWare.NUnitTestRunner

/// A module deliberately carrying no NUnit attributes: NUnit must not discover it, because its lifecycle methods
/// fail. Instead, TestRunFiltering drives it through TestFixture.run.
module ExcludedByFilterFixture =
    /// Lets us name this module's compiled Type via typeof.
    type Marker = class end

    let oneTimeSetUpRunCount = ref 0
    let oneTimeTearDownRunCount = ref 0

    let oneTimeSetUp () : unit =
        Interlocked.Increment oneTimeSetUpRunCount |> ignore<int>
        failwith "deliberately failing one-time setup"

    let oneTimeTearDown () : unit =
        Interlocked.Increment oneTimeTearDownRunCount |> ignore<int>
        failwith "deliberately failing one-time tear-down"

    let someTest () : unit = ()

[<TestFixture>]
module TestRunFiltering =

    [<Test>]
    let ``A fixture none of whose tests are selected does not run its lifecycle methods`` () =
        let moduleType = typeof<ExcludedByFilterFixture.Marker>.DeclaringType

        let test : SingleTestMethod =
            {
                Method = moduleType.GetMethod (nameof ExcludedByFilterFixture.someTest)
                Kind = TestKind.Single
                Modifiers = []
                Categories = []
                Repeat = None
                Combinatorial = None
                Parallelize = None
            }

        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                Tests = [ test ]
                OneTimeSetUp = Some (moduleType.GetMethod (nameof ExcludedByFilterFixture.oneTimeSetUp))
                OneTimeTearDown = Some (moduleType.GetMethod (nameof ExcludedByFilterFixture.oneTimeTearDown))
            }

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

        let results =
            (TestFixture.run contexts par progress (fun _ _ -> false) fixture).Result

        ExcludedByFilterFixture.oneTimeSetUpRunCount.Value |> shouldEqual 0
        ExcludedByFilterFixture.oneTimeTearDownRunCount.Value |> shouldEqual 0

        skipped |> Seq.toList |> shouldEqual [ nameof ExcludedByFilterFixture.someTest ]

        let results = results |> List.exactlyOne
        results.OtherFailures |> shouldBeEmpty
        results.Failed |> shouldBeEmpty
        results.Success |> shouldBeEmpty
