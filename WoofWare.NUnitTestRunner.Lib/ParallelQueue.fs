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

type private MailboxMessage =
    | Quit of AsyncReplyChannel<unit>
    | RunTest of ThunkCrate
    | BeginTestFixture of TestFixture * AsyncReplyChannel<TestFixtureRunningToken>
    | EndTestFixture of TestFixtureRunningToken * AsyncReplyChannel<unit>

type private MailboxState =
    | Idle
    | Running of TestFixture * (TestFixture * AsyncReplyChannel<TestFixtureRunningToken>) list

/// Run some things in parallel.
/// TODO: actually implement the parallelism! Right now this just runs everything serially.
/// TODO: consume the cancellation token
type ParallelQueue
    (_parallelism : int option, _scope : Parallelizable<AssemblyParallelScope> option, ?ct : CancellationToken)
    =
    let rec processTask (state : MailboxState) (m : MailboxProcessor<MailboxMessage>) =
        async {
            let! message = m.Receive ()

            match message with
            | Quit rc -> rc.Reply ()
            | BeginTestFixture (tf, rc) ->
                match state with
                | Running (current, rest) ->
                    let state = Running (current, (tf, rc) :: rest)
                    return! processTask state m
                | Idle ->
                    let state = Running (tf, [])
                    rc.Reply (TestFixtureRunningToken tf)
                    return! processTask state m
            | EndTestFixture (TestFixtureRunningToken tf, rc) ->
                match state with
                | Idle ->
                    return failwith "Caller has somehow called EndTestFixture while we're not running a test fixture"
                | Running (current, rest) ->
                    if not (Object.ReferenceEquals (current, tf)) then
                        return
                            failwith
                                "Caller has somehow called EndTestFixture while we're not running that test fixture"

                    rc.Reply ()

                    match rest with
                    | [] -> return! processTask Idle m
                    | (head, rc) :: tail ->
                        rc.Reply (TestFixtureRunningToken head)
                        return! processTask (Running (head, tail)) m
            | RunTest message ->
                // Currently we rely on the caller to only send this message when we've given them permission through
                // the StartTestFixture method returning.
                { new ThunkEvaluator<_> with
                    member _.Eval t rc =
                        t () |> rc.Reply
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
    member _.NonParallel<'a> (action : unit -> 'a) : 'a Task =
        ThunkCrate.make action >> RunTest |> mb.PostAndAsyncReply |> Async.StartAsTask

    /// Request to run the given action, freely in parallel with other running tests.
    /// The resulting Task will return when the action has completed.
    member _.Parallel<'a> (action : unit -> 'a) : 'a Task =
        ThunkCrate.make action >> RunTest |> mb.PostAndAsyncReply |> Async.StartAsTask

    /// Request to run the given action, obeying the parallelism constraints of the parent test fixture.
    /// The resulting Task will return when the action has completed.
    member _.ObeyParent<'a> (tf : TestFixtureRunningToken) (action : unit -> 'a) : 'a Task =
        ThunkCrate.make action >> RunTest |> mb.PostAndAsyncReply |> Async.StartAsTask

    /// Declare that we wish to start the given test fixture. The resulting Task will return
    /// when you are allowed to start running tests from that fixture.
    /// Once you've finished running tests from that fixture, call EndTestFixture.
    member _.StartTestFixture (tf : TestFixture) : Task<TestFixtureRunningToken> =
        fun rc -> BeginTestFixture (tf, rc)
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Declare that we have finished submitting requests to run in the given test fixture.
    /// You don't need to worry about when the resulting Task returns, but we provide it just in case.
    member _.EndTestFixture (tf : TestFixtureRunningToken) : Task<unit> =
        (fun rc -> EndTestFixture (tf, rc)) |> mb.PostAndAsyncReply |> Async.StartAsTask

    interface IDisposable with
        member _.Dispose () =
            // Still race conditions, of course: people could still be submitting after we finish the sync.
            mb.PostAndReply Quit
            (mb :> IDisposable).Dispose ()
