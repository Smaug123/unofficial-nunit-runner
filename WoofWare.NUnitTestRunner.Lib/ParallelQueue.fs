namespace WoofWare.NUnitTestRunner

open System
open System.Threading
open System.Threading.Tasks

type private ThunkEvaluator<'ret> =
    abstract Eval<'a> : (unit -> 'a) -> AsyncReplyChannel<'a> -> 'ret

type private ThunkCrate =
    abstract Apply<'ret> : ThunkEvaluator<'ret> -> 'ret

[<RequireQualifiedAccess>]
module private ThunkCrate =
    let make<'a> (t : unit -> 'a) (rc : AsyncReplyChannel<'a>) : ThunkCrate =
        { new ThunkCrate with
            member _.Apply e = e.Eval t rc
        }

type private FakeUnit = FakeUnit

/// A handle to a running test fixture.
type TestFixtureRunningToken = private | TestFixtureRunningToken of TestFixture

/// A handle to a test fixture whose setup method has been called.
type TestFixtureSetupToken = private | TestFixtureSetupToken of TestFixture

[<RequireQualifiedAccess>]
module private TestFixtureSetupToken =
    let vouchNoSetupRequired (TestFixtureRunningToken tf) = TestFixtureSetupToken tf

/// A handle to a test fixture whose setup method has been called.
type TestFixtureTearDownToken = private | TestFixtureTearDownToken of TestFixture

[<RequireQualifiedAccess>]
module private TestFixtureTearDownToken =
    let vouchNoTearDownRequired (TestFixtureSetupToken tf) = TestFixtureTearDownToken tf

type private MailboxMessage =
    | Quit of AsyncReplyChannel<unit>
    | RunTest of within : TestFixture * test : ThunkCrate
    | BeginTestFixture of TestFixture * AsyncReplyChannel<TestFixtureRunningToken>
    | EndTestFixture of TestFixtureTearDownToken * AsyncReplyChannel<unit>

type private RunningState =
    {
        MaxParallelism : int
        // TODO: make these efficiently look-up-able
        CurrentlyRunning : (TestFixture * ThunkCrate list) list
        Waiting : (TestFixture * AsyncReplyChannel<TestFixtureRunningToken>) list
    }

    member this.CompleteFixture (tf : TestFixture) : RunningState =
        let rec go (acc : (TestFixture * ThunkCrate list) list) (running : (TestFixture * ThunkCrate list) list) =
            match running with
            | [] -> failwith "Caller has somehow called EndTestFixture while we're not running that test fixture"
            | (head, running) :: tail ->
                if Object.ReferenceEquals (head, tf) then
                    match running with
                    | [] -> acc @ tail
                    | _ ->
                        failwith
                            $"Caller has called EndTestFixture while its tests are still running (%i{running.Length})"
                else
                    go ((head, running) :: acc) tail

        let currentlyRunning = go [] this.CurrentlyRunning

        {
            CurrentlyRunning = currentlyRunning
            Waiting = this.Waiting
            MaxParallelism = this.MaxParallelism
        }

type private MailboxState =
    | Idle
    | Running of RunningState

/// Run some things in parallel.
/// TODO: actually implement the parallelism! Right now this just runs everything serially.
/// TODO: consume the cancellation token
type ParallelQueue
    (parallelism : int option, _scope : Parallelizable<AssemblyParallelScope> option, ?ct : CancellationToken)
    =
    let parallelism =
        match parallelism with
        | None -> max (Environment.ProcessorCount / 2) 2
        | Some p -> p

    let rec processTask (state : MailboxState) (m : MailboxProcessor<MailboxMessage>) =
        async {
            let! message = m.Receive ()

            match message with
            | Quit rc -> rc.Reply ()
            | BeginTestFixture (tf, rc) ->
                match state with
                | Running state ->
                    let state =
                        {
                            MaxParallelism = state.MaxParallelism
                            CurrentlyRunning = state.CurrentlyRunning
                            Waiting = (tf, rc) :: state.Waiting
                        }
                        |> Running

                    return! processTask state m
                | Idle ->
                    let state =
                        {
                            MaxParallelism = parallelism
                            CurrentlyRunning = [ tf, [] ]
                            Waiting = []
                        }
                        |> Running

                    rc.Reply (TestFixtureRunningToken tf)
                    return! processTask state m
            | EndTestFixture (TestFixtureTearDownToken tf, rc) ->
                match state with
                | Idle ->
                    return failwith "Caller has somehow called EndTestFixture while we're not running a test fixture"
                | Running state ->
                    let state = state.CompleteFixture tf

                    rc.Reply ()

                    match state.Waiting with
                    | [] -> return! processTask Idle m
                    | (head, rc) :: tail ->
                        rc.Reply (TestFixtureRunningToken head)

                        let state =
                            {
                                MaxParallelism = state.MaxParallelism
                                CurrentlyRunning = (head, []) :: state.CurrentlyRunning
                                Waiting = tail
                            }

                        return! processTask (Running state) m
            | RunTest (withinFixture, message) ->
                // Currently we rely on the caller to only send this message when we've given them permission through
                // the StartTestFixture method returning.
                { new ThunkEvaluator<_> with
                    member _.Eval t rc =
                        use ec = ExecutionContext.Capture ()
                        ExecutionContext.Run (ec, (fun _ -> rc.Reply (t ())), ())
                        FakeUnit
                }
                |> message.Apply
                |> function
                    | FakeUnit -> ()

                return! processTask state m
        }

    let mb = new MailboxProcessor<_> (processTask MailboxState.Idle)
    do mb.Start ()

    /// Request to run the given action on its own, not in parallel with anything else.
    /// The resulting Task will return when the action has completed.
    member _.NonParallel<'a> (TestFixtureSetupToken parent) (action : unit -> 'a) : 'a Task =
        (fun rc -> RunTest (parent, ThunkCrate.make action rc))
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Request to run the given action, freely in parallel with other running tests.
    /// The resulting Task will return when the action has completed.
    member _.Parallel<'a> (TestFixtureSetupToken parent) (action : unit -> 'a) : 'a Task =
        (fun rc -> RunTest (parent, ThunkCrate.make action rc))
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Request to run the given action, obeying the parallelism constraints of the parent test fixture.
    /// The resulting Task will return when the action has completed.
    member _.ObeyParent<'a> (TestFixtureSetupToken parent) (action : unit -> 'a) : 'a Task =
        (fun rc -> RunTest (parent, ThunkCrate.make action rc))
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Declare that we wish to start the given test fixture. The resulting Task will return
    /// when you are allowed to start running tests from that fixture.
    /// Once you've finished running tests from that fixture, call EndTestFixture.
    member _.StartTestFixture (tf : TestFixture) : Task<TestFixtureRunningToken> =
        fun rc -> BeginTestFixture (tf, rc)
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Run the given one-time setup for the test fixture.
    member _.RunTestSetup (TestFixtureRunningToken parent) (action : unit -> 'a) : ('a * TestFixtureSetupToken) Task =
        task {
            let! response = (fun rc -> RunTest (parent, ThunkCrate.make action rc)) |> mb.PostAndAsyncReply
            return response, TestFixtureSetupToken parent
        }

    /// Run the given one-time tear-down for the test fixture.
    member _.RunTestTearDown
        (TestFixtureSetupToken parent)
        (action : unit -> 'a)
        : ('a * TestFixtureTearDownToken) Task
        =
        task {
            let! response = (fun rc -> RunTest (parent, ThunkCrate.make action rc)) |> mb.PostAndAsyncReply
            return response, TestFixtureTearDownToken parent
        }

    /// Declare that we have finished submitting requests to run in the given test fixture.
    /// You don't need to worry about when the resulting Task returns, but we provide it just in case.
    member _.EndTestFixture (tf : TestFixtureTearDownToken) : Task<unit> =
        (fun rc -> EndTestFixture (tf, rc)) |> mb.PostAndAsyncReply |> Async.StartAsTask

    interface IDisposable with
        member _.Dispose () =
            // Still race conditions, of course: people could still be submitting after we finish the sync.
            mb.PostAndReply Quit
            (mb :> IDisposable).Dispose ()
