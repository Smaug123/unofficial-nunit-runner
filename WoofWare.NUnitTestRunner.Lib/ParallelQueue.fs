namespace WoofWare.NUnitTestRunner

open System
open System.Threading
open System.Threading.Tasks

type private ThunkEvaluator<'ret> =
    abstract Eval<'a> : (unit -> 'a) -> AsyncReplyChannel<Result<'a, exn>> -> 'ret

type private ThunkCrate =
    abstract Apply<'ret> : ThunkEvaluator<'ret> -> 'ret

[<RequireQualifiedAccess>]
module private ThunkCrate =
    let make<'a> (t : unit -> 'a) (rc : AsyncReplyChannel<Result<'a, exn>>) : ThunkCrate =
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

[<RequireQualifiedAccess>]
type private MailboxMessage =
    | Quit of AsyncReplyChannel<unit>
    /// Check current state, see if we need to start more tests, etc.
    | Reconcile
    | RunTest of within : TestFixture * Parallelizable<unit> option * test : ThunkCrate * context : ExecutionContext
    | BeginTestFixture of TestFixture * AsyncReplyChannel<TestFixtureRunningToken>
    | EndTestFixture of TestFixtureTearDownToken * AsyncReplyChannel<unit>

type private RunningFixture =
    {
        Fixture : TestFixture
        RunningCanParallelize : bool
        Running : Task list
        Waiting : ((unit -> Task) * Parallelizable<unit> option) list
    }

    static member Make (f : TestFixture) =
        {
            Fixture = f
            Running = []
            RunningCanParallelize = true
            Waiting = []
        }

type private RunningState =
    {
        MaxParallelism : int
        // TODO: make these efficiently look-up-able
        CurrentlyRunning : RunningFixture list
        Waiting : (TestFixture * AsyncReplyChannel<TestFixtureRunningToken>) list
    }

    member this.NewTest (tf : TestFixture) (par : Parallelizable<unit> option) (test : unit -> Task) =
        {
            MaxParallelism = this.MaxParallelism
            Waiting = this.Waiting
            CurrentlyRunning =
                let found = ref 0

                this.CurrentlyRunning
                |> List.map (fun f ->
                    if Object.ReferenceEquals (f.Fixture, tf) then
                        Interlocked.Increment found |> ignore<int>

                        { f with
                            Waiting = (test, par) :: f.Waiting
                        }
                    else
                        f
                )
                |> fun l ->
                    match found.Value with
                    | 1 -> l
                    | 0 -> failwith $"Unexpectedly did not find the running test fixture '%s{tf.Name}' to add a test to"
                    | _ -> failwith $"Unexpectedly found the running test fixture '%s{tf.Name}' multiple times in list"
        }

    member this.CompleteFixture (tf : TestFixture) : RunningState =
        let rec go (acc : RunningFixture list) (running : RunningFixture list) =
            match running with
            | [] -> failwith "Caller has somehow called EndTestFixture while we're not running that test fixture"
            | runningFixture :: tail ->
                if Object.ReferenceEquals (runningFixture.Fixture, tf) then
                    match runningFixture.Running, runningFixture.Waiting with
                    | [], [] -> acc @ tail
                    | r, [] ->
                        failwith $"Caller has called EndTestFixture while its tests are still running (%i{r.Length})"
                    | [], r ->
                        failwith $"Caller has called EndTestFixture while it has tests waiting to run (%i{r.Length})"
                    | r, s ->
                        failwith
                            $"Caller has called EndTestFixture while it has tests waiting to run (%i{s.Length}) and test running (%i{r.Length})"
                else
                    go (runningFixture :: acc) tail

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
            | MailboxMessage.Quit rc -> rc.Reply ()
            | MailboxMessage.Reconcile ->
                match state with
                | Idle -> return! processTask state m
                | Running r ->

                match r.CurrentlyRunning with
                | [] ->
                    match r.Waiting with
                    | [] -> return! processTask Idle m
                    | (head, rc) :: tail ->
                        rc.Reply (TestFixtureRunningToken head)

                        let newRunning =
                            {
                                Fixture = head
                                Running = []
                                RunningCanParallelize = true
                                Waiting = []
                            }

                        let state =
                            {
                                MaxParallelism = r.MaxParallelism
                                CurrentlyRunning = [ newRunning ]
                                Waiting = tail
                            }
                        // For now, we'll just run one fixture at a time. When we run multiple fixtures in parallel,
                        // we probably want to call Reconcile here again.
                        return! processTask (Running state) m
                | [ currentlyRunning ] ->
                    let currentlyRunningTasks =
                        currentlyRunning.Running |> List.filter (fun t -> not t.IsCompleted)

                    let r =
                        { r with
                            CurrentlyRunning =
                                [
                                    { currentlyRunning with
                                        Running = currentlyRunningTasks
                                    }
                                ]
                        }

                    match currentlyRunningTasks with
                    | [] ->
                        match currentlyRunning.Waiting with
                        | [] ->
                            // Nothing to run yet
                            return! processTask (Running r) m
                        | (head, par) :: tail ->
                            let par =
                                match par with
                                | None -> true
                                | Some Parallelizable.No -> false
                                | Some (Parallelizable.Yes ()) -> true

                            let state =
                                {
                                    Fixture = currentlyRunning.Fixture
                                    RunningCanParallelize = par
                                    Waiting = tail
                                    Running = [ head () ]
                                }

                            m.Post MailboxMessage.Reconcile

                            return!
                                processTask
                                    (Running
                                        { r with
                                            CurrentlyRunning = [ state ]
                                        })
                                    m

                    | currentlyRunningTasks ->

                    if currentlyRunningTasks.Length >= parallelism then
                        return! processTask (Running r) m
                    else

                    match currentlyRunning.Waiting, currentlyRunning.RunningCanParallelize with
                    | [], _ ->
                        // No new candidates.
                        return! processTask (Running r) m
                    | _, false ->
                        // The running test(s) can't have others added.
                        return! processTask (Running r) m
                    | (head, par) :: tail, true ->
                        match par with
                        | Some Parallelizable.No -> return! processTask (Running r) m
                        | Some (Parallelizable.Yes ()) ->
                            let state =
                                {
                                    RunningState.MaxParallelism = r.MaxParallelism
                                    Waiting = r.Waiting
                                    CurrentlyRunning =
                                        [
                                            {
                                                Fixture = currentlyRunning.Fixture
                                                RunningCanParallelize = true
                                                Running = head () :: currentlyRunning.Running
                                                Waiting = tail
                                            }
                                        ]
                                }

                            m.Post MailboxMessage.Reconcile
                            return! processTask (Running state) m
                        | None ->
                            match currentlyRunning.Fixture.Parallelize with
                            | Some Parallelizable.No
                            | Some (Parallelizable.Yes ClassParallelScope.Self)
                            | Some (Parallelizable.Yes ClassParallelScope.Fixtures) ->
                                // Can't add this test to the parallel queue right now
                                return! processTask (Running r) m
                            | None
                            | Some (Parallelizable.Yes ClassParallelScope.All)
                            | Some (Parallelizable.Yes ClassParallelScope.Children) ->
                                let state =
                                    {
                                        Fixture = currentlyRunning.Fixture
                                        RunningCanParallelize = true
                                        Waiting = tail
                                        Running = (head ()) :: currentlyRunningTasks
                                    }

                                m.Post MailboxMessage.Reconcile

                                return!
                                    processTask
                                        (Running
                                            { r with
                                                CurrentlyRunning = [ state ]
                                            })
                                        m
                | _ -> failwith "Logic error: we currently only run one fixture at a time"
            | MailboxMessage.BeginTestFixture (tf, rc) ->
                match state with
                | Running state ->
                    let state =
                        {
                            MaxParallelism = state.MaxParallelism
                            CurrentlyRunning = state.CurrentlyRunning
                            Waiting = (tf, rc) :: state.Waiting
                        }
                        |> Running

                    m.Post MailboxMessage.Reconcile
                    return! processTask state m
                | Idle ->
                    let state =
                        {
                            MaxParallelism = parallelism
                            CurrentlyRunning = []
                            Waiting = [ (tf, rc) ]
                        }
                        |> Running

                    m.Post MailboxMessage.Reconcile
                    return! processTask state m
            | MailboxMessage.EndTestFixture (TestFixtureTearDownToken tf, rc) ->
                match state with
                | Idle ->
                    return failwith "Caller has somehow called EndTestFixture while we're not running a test fixture"
                | Running state ->
                    let state = state.CompleteFixture tf
                    rc.Reply ()
                    m.Post MailboxMessage.Reconcile
                    return! processTask (Running state) m
            | MailboxMessage.RunTest (withinFixture, par, message, capturedContext) ->
                let t () =
                    { new ThunkEvaluator<_> with
                        member _.Eval<'b> (t : unit -> 'b) rc =
                            let tcs = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously

                            fun () ->
                                ExecutionContext.Run (
                                    capturedContext,
                                    (fun _ ->
                                        let result =
                                            try
                                                let r = t ()
                                                Ok r
                                            with e ->
                                                Error e

                                        tcs.SetResult ()
                                        m.Post MailboxMessage.Reconcile
                                        rc.Reply result
                                    ),
                                    ()
                                )
                            |> Task.Factory.StartNew
                            |> ignore<Task>

                            tcs.Task
                    }
                    |> message.Apply

                let state =
                    match state with
                    | Idle -> failwith "somehow asked the queue to run tests when there is no active fixture"
                    | Running state -> state.NewTest withinFixture par t |> Running

                m.Post MailboxMessage.Reconcile

                return! processTask state m
        }

    let mb = new MailboxProcessor<_> (processTask MailboxState.Idle)
    do mb.Start ()

    /// Request to run the given action, freely in parallel with other running tests.
    /// The resulting Task will return when the action has completed.
    member _.Run<'a>
        (TestFixtureSetupToken parent)
        (scope : Parallelizable<unit> option)
        (action : unit -> 'a)
        : 'a Task
        =
        let ec = ExecutionContext.Capture ()

        task {
            let! result =
                (fun rc -> MailboxMessage.RunTest (parent, scope, ThunkCrate.make action rc, ec))
                |> mb.PostAndAsyncReply
                |> Async.StartAsTask

            match result with
            | Ok o -> return o
            | Error e -> return Exception.reraiseWithOriginalStackTrace e
        }

    /// Declare that we wish to start the given test fixture. The resulting Task will return
    /// when you are allowed to start running tests from that fixture.
    /// Once you've finished running tests from that fixture, call EndTestFixture.
    member _.StartTestFixture (tf : TestFixture) : Task<TestFixtureRunningToken> =
        fun rc -> MailboxMessage.BeginTestFixture (tf, rc)
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    /// Run the given one-time setup for the test fixture.
    member _.RunTestSetup (TestFixtureRunningToken parent) (action : unit -> 'a) : ('a * TestFixtureSetupToken) Task =
        task {
            let par =
                parent.Parallelize
                |> Option.map (fun p ->
                    match p with
                    | Parallelizable.No -> Parallelizable.No
                    | Parallelizable.Yes _ -> Parallelizable.Yes ()
                )

            let ec = ExecutionContext.Capture ()

            let! response =
                (fun rc -> MailboxMessage.RunTest (parent, par, ThunkCrate.make action rc, ec))
                |> mb.PostAndAsyncReply

            match response with
            | Ok response -> return response, TestFixtureSetupToken parent
            | Error e -> return Exception.reraiseWithOriginalStackTrace e
        }

    /// Run the given one-time tear-down for the test fixture.
    member _.RunTestTearDown
        (TestFixtureSetupToken parent)
        (action : unit -> 'a)
        : ('a * TestFixtureTearDownToken) Task
        =
        task {
            let par =
                parent.Parallelize
                |> Option.map (fun p ->
                    match p with
                    | Parallelizable.No -> Parallelizable.No
                    | Parallelizable.Yes _ -> Parallelizable.Yes ()
                )

            let ec = ExecutionContext.Capture ()

            let! response =
                (fun rc -> MailboxMessage.RunTest (parent, par, ThunkCrate.make action rc, ec))
                |> mb.PostAndAsyncReply

            match response with
            | Ok response -> return response, TestFixtureTearDownToken parent
            | Error e -> return Exception.reraiseWithOriginalStackTrace e
        }

    /// Declare that we have finished submitting requests to run in the given test fixture.
    /// You don't need to worry about when the resulting Task returns, but we provide it just in case.
    member _.EndTestFixture (tf : TestFixtureTearDownToken) : Task<unit> =
        (fun rc -> MailboxMessage.EndTestFixture (tf, rc))
        |> mb.PostAndAsyncReply
        |> Async.StartAsTask

    interface IDisposable with
        member _.Dispose () =
            // Still race conditions, of course: people could still be submitting after we finish the sync.
            mb.PostAndReply MailboxMessage.Quit
            (mb :> IDisposable).Dispose ()
