namespace WoofWare.NUnitTestRunner.Test

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnitTyped
open WoofWare.NUnitTestRunner

/// A module deliberately carrying no NUnit attributes: NUnit must not discover it, because its members
/// misbehave. Instead, TestAsyncLifecycle drives it through TestFixture.run.
module ResultBearingFixture =
    /// Lets us name this module's compiled Type via typeof.
    type Marker = class end

    let setUpBodyRuns = ref 0

    let taskOfIntSetUp () : Task<int> =
        Interlocked.Increment setUpBodyRuns |> ignore<int>
        Task.FromResult 3

    let taskOfIntTest () : Task<int> = Task.FromResult 3

    let taskOfUnitTest () = task { return () }

    let someTest () : unit = ()

    let cancellingTest () : Async<unit> =
        async {
            // Hostile user code: this cancels the process-wide default token. The runner must not have
            // anything running on that token.
            Async.CancelDefaultToken ()
            do! Async.Sleep 1000
        }

    let sleepingTest () : Async<unit> = async { do! Async.Sleep 10000 }

[<TestFixture>]
module TestAsyncLifecycle =

    let private noOpProgress =
        { new ITestProgress with
            member _.OnTestFixtureStart _ _ = ()
            member _.OnTestFixtureSkipped _ _ = ()
            member _.OnTestMemberStart _ = ()
            member _.OnTestFailed _ _ = ()
            member _.OnTestMemberFinished _ = ()
            member _.OnTestMemberSkipped _ = ()
        }

    let private moduleType = typeof<ResultBearingFixture.Marker>.DeclaringType

    let private makeTest (name : string) : SingleTestMethod =
        {
            Method = moduleType.GetMethod name
            Kind = TestKind.Single
            Modifiers = []
            Categories = []
            Repeat = None
            Combinatorial = None
            Parallelize = None
        }

    let private runFixture (fixture : TestFixture) : FixtureRunResults =
        use contexts = TestContexts.Empty ()
        use par = new ParallelQueue (None, None)

        let run = TestFixture.run contexts par noOpProgress (fun _ _ -> true) fixture

        if not (run.Wait (TimeSpan.FromSeconds 60.0)) then
            failwith "test run failed to terminate"

        run.Result |> List.exactlyOne

    [<Test>]
    let ``A one-time setup returning Task of int is rejected without being run`` () =
        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                OneTimeSetUp = Some (moduleType.GetMethod (nameof ResultBearingFixture.taskOfIntSetUp))
                Tests = [ makeTest (nameof ResultBearingFixture.someTest) ]
            }

        let results = runFixture fixture

        results.Success |> shouldBeEmpty
        results.Failed |> shouldBeEmpty

        match results.OtherFailures with
        | [ UserMethodFailure.ReturnedNonUnit _, _ ] -> ()
        | other -> failwith $"Expected exactly one ReturnedNonUnit failure, got: %O{other}"

        // NUnit rejects such a method without executing it, and so do we.
        ResultBearingFixture.setUpBodyRuns.Value |> shouldEqual 0

    [<Test>]
    let ``A test returning Task of int is rejected; a test returning Task of unit is accepted`` () =
        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                Tests =
                    [
                        makeTest (nameof ResultBearingFixture.taskOfIntTest)
                        makeTest (nameof ResultBearingFixture.taskOfUnitTest)
                    ]
            }

        let results = runFixture fixture

        results.OtherFailures |> shouldBeEmpty

        results.Success
        |> List.map (fun (test, result, _) -> test.Name, result)
        |> shouldEqual [ nameof ResultBearingFixture.taskOfUnitTest, TestMemberSuccess.Ok ]

        match results.Failed with
        | [ TestMemberFailure.Failed [ TestFailure.TestFailed (UserMethodFailure.ReturnedNonUnit _) ], _ ] -> ()
        | other -> failwith $"Expected exactly one ReturnedNonUnit test failure, got: %O{other}"

    [<Test>]
    let ``Cancelling the default token from user code does not deadlock the runner`` () =
        let fixture =
            { TestFixture.Empty moduleType None [] [] with
                Tests = [ makeTest (nameof ResultBearingFixture.cancellingTest) ]
            }

        let results = runFixture fixture

        results.OtherFailures |> shouldBeEmpty
        results.Failed |> shouldBeEmpty

        // The runner's own machinery must not be running on the token the user cancelled, so the user's
        // async just runs to completion.
        results.Success
        |> List.map (fun (test, result, _) -> test.Name, result)
        |> shouldEqual [ nameof ResultBearingFixture.cancellingTest, TestMemberSuccess.Ok ]

    [<Test>]
    let ``A cancelled user async is reported as a real exception`` () =
        let method = moduleType.GetMethod (nameof ResultBearingFixture.sleepingTest)

        use cts = new CancellationTokenSource ()
        cts.CancelAfter (TimeSpan.FromMilliseconds 50.0)

        let result =
            Async.RunSynchronously (
                TestFixture.runUserMethod cts.Token (null : obj) [||] method,
                cancellationToken = CancellationToken.None
            )

        match result with
        | Error (UserMethodFailure.Threw (_, exc)) ->
            // A cancelled task has Exception = null; the failure must carry the real
            // TaskCanceledException, or downstream report generation dereferences null and crashes.
            isNull (box exc) |> shouldEqual false
        | other -> failwith $"Expected a Threw failure, got: %O{other}"
