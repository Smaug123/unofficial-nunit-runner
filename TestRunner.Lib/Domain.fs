namespace TestRunner

open System.Reflection

/// A modifier on whether a given test should be run.
[<RequireQualifiedAccess>]
type Modifier =
    /// This test is Explicit: it can only be run by an explicit instruction to do so.
    /// (As of this writing, the console runner will never run such tests.)
    | Explicit of reason : string option
    /// This test is Ignored: it will never be run by the harness.
    | Ignored of reason : string option

/// Describes where data comes from, if any, to provide the args to this test.
[<RequireQualifiedAccess>]
type TestKind =
    /// This test takes no arguments.
    | Single
    /// This test has arguments supplied by TestCaseSource (i.e. we look for members with the given names, and
    /// populate the args from those).
    | Source of string list
    /// This test has arguments supplied by TestCase attributes.
    | Data of obj list list

/// Determines whether a set of `[<Value>]`s will be combined elementwise or Cartesian-product-wise.
type Combinatorial =
    /// Combine `[<Value>]`s to produce every possible combination of args drawn from the respective sets.
    | Combinatorial
    /// Combine `[<Value>]`s such that one test is "the first Value from each", one test is "the second Value from
    /// each", and so on. Spare slots are filled with `Unchecked.defaultof<_>`.
    | Sequential

/// A single method or member which holds some tests. (Often such a member will represent only one test, but e.g.
/// if it has [<TestCaseSource>] then it represents multiple tests.)
type SingleTestMethod =
    {
        /// The method which we need to invoke, possibly some args, to run the test.
        Method : MethodInfo
        /// Where the data comes from to populate the args for this method.
        Kind : TestKind
        /// Any statements about whether the runner should run this test.
        /// (This does not include use of `--filter`s.)
        Modifiers : Modifier list
        /// `[<Category>]`s this test is in.
        Categories : string list
        /// Whether we should run this test repeatedly, and if so, how many times.
        Repeat : int option
        /// If this test has data supplied by `[<Value>]` annotations, specifies how those annotations are combined
        /// to produce the complete collection of args.
        Combinatorial : Combinatorial option
    }

    /// Human-readable name of this test method.
    member this.Name = this.Method.Name

/// A test fixture (usually represented by the [<TestFixture>]` attribute), which may contain many tests,
/// each of which may run many times.
type TestFixture =
    {
        /// Fully-qualified name of this fixture (e.g. MyThing.Test.Foo for `[<TestFixture>] module Foo` in the
        /// `MyThing.Test` assembly).
        Name : string
        /// A method which is run once when this test fixture starts, before any other setup logic and before
        /// any tests run. If this method fails, no tests will run and no per-test setup/teardown logic will run,
        /// but OneTimeTearDown will run.
        OneTimeSetUp : MethodInfo option
        /// A method which is run once, after any other tear-down logic and after all tests run, even if everything
        /// else failed before this (i.e. even if OneTimeSetUp failed, even if all tests failed, etc).
        OneTimeTearDown : MethodInfo option
        /// Methods which are run in some arbitrary order before each individual test. If any of these fail, the test
        /// will not run, but the TearDown methods will still run, and OneTimeTearDown will still run at the end of
        /// the fixture. If the first SetUp we run fails, we don't define whether the other SetUps run before
        /// we proceed directly to running all the TearDowns.
        SetUp : MethodInfo list
        /// Methods which are run in some arbitrary order after each individual test, even if the test or its setup
        /// failed. If the first TearDown we run fails, we don't define whether the other TearDowns run.
        TearDown : MethodInfo list
        /// The individual test methods present within this fixture.
        Tests : SingleTestMethod list
    }

    /// A test fixture about which we know nothing.
    static member Empty (name : string) =
        {
            Name = name
            OneTimeSetUp = None
            OneTimeTearDown = None
            SetUp = []
            TearDown = []
            Tests = []
        }

/// User code in the unit under test has failed somehow.
[<RequireQualifiedAccess>]
type UserMethodFailure =
    /// A method ran to completion and returned a value, when it was expected to return nothing.
    | ReturnedNonUnit of name : string * result : obj
    /// A method threw.
    | Threw of name : string * exn

    /// Human-readable representation of the user failure.
    override this.ToString () =
        match this with
        | UserMethodFailure.ReturnedNonUnit (method, ret) ->
            $"User-defined method %s{method} returned a non-unit: %O{ret}"
        | UserMethodFailure.Threw (method, exc) ->
            $"User-defined method %s{method} threw: %s{exc.Message}\n  %s{exc.StackTrace}"

/// Represents the failure of a single run of one test. An error signalled this way is a user error: the unit under
/// test has misbehaved.
[<RequireQualifiedAccess>]
type TestFailure =
    /// The test itself failed. (Setup must have succeeded if you get this.)
    | TestFailed of UserMethodFailure
    /// We failed to set up the test (e.g. its SetUp failed). If this happens, we won't proceed
    /// to running the test or running any TearDown for that test.
    | SetUpFailed of UserMethodFailure
    /// We failed to tear down the test (e.g. its TearDown failed). This can happen even if the test failed,
    /// because we always run tear-downs, even after failed tests.
    | TearDownFailed of UserMethodFailure
