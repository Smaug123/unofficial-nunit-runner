namespace TestRunner

open System

/// Represents something which knows how to report progress through a test suite.
/// Note that we don't guarantee anything about parallelism; you must make sure
/// all implementations are safe to run concurrently.
type ITestProgress =
    /// Called just before we start executing the setup logic for the given test fixture.
    /// We tell you how many test methods there are in the fixture.
    abstract OnTestFixtureStart : name : string -> testCount : int -> unit
    /// Called just before we start executing the test(s) indicated by a particular method.
    abstract OnTestMemberStart : name : string -> unit
    /// Called when a test fails. (This may be called repeatedly with the same `name`, e.g. if the test
    /// is run multiple times with different combinations of test data.)
    abstract OnTestFailed : name : string -> failure : TestMemberFailure -> unit
    /// Called when we've finished every test indicated by a particular method. (The test may have been run
    /// multiple times, e.g. with different combinations of test data.)
    abstract OnTestMemberFinished : name : string -> unit
    /// Called when we decide not to run the test(s) indicated by a particular method (e.g. because it's
    /// marked [<Explicit>]).
    abstract OnTestMemberSkipped : name : string -> unit

/// Methods for constructing specific ITestProgress objects.
[<RequireQualifiedAccess>]
module TestProgress =
    /// An ITestProgress which logs to stderr.
    let toStderr () : ITestProgress =
        { new ITestProgress with
            member _.OnTestFixtureStart name testCount =
                let plural = if testCount = 1 then "" else "s"
                Console.Error.WriteLine $"Running test fixture: %s{name} (%i{testCount} test%s{plural} to run)"

            member _.OnTestMemberStart name =
                Console.Error.WriteLine $"Running test: %s{name}"

            member _.OnTestFailed name failure =
                Console.Error.WriteLine $"Test failed: %O{failure}"

            member _.OnTestMemberFinished name =
                Console.Error.WriteLine $"Finished test %s{name}"

            member _.OnTestMemberSkipped name =
                Console.Error.WriteLine $"Skipping test due to filter: %s{name}"
        }
