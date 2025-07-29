namespace WoofWare.NUnitTestRunner.Test

open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnitTyped
open WoofWare.NUnitTestRunner

[<TestFixture>]
module TestParallelQueueContext =

    [<Test>]
    let ``ExecutionContext flows correctly through synchronous operations`` () =
        task {
            let dummyFixture =
                TestFixture.Empty typeof<obj> (Some (Parallelizable.Yes ClassParallelScope.All)) [] []

            use contexts = TestContexts.Empty ()
            use queue = new ParallelQueue (Some 4, None)

            // Track which context values we see during execution
            let contextValues = System.Collections.Concurrent.ConcurrentBag<Guid * Guid> ()

            // Start the fixture
            let! running = queue.StartTestFixture dummyFixture
            let! _, setup = queue.RunTestSetup running (fun () -> ())

            // Create several synchronous operations with different context values
            let tasks =
                [ 1..10 ]
                |> List.map (fun i ->
                    task {
                        do! Task.Yield ()
                        // Set a unique context value
                        let expectedId = Guid.NewGuid ()
                        let outputId = OutputStreamId expectedId
                        contexts.AsyncLocal.Value <- outputId

                        // Run a synchronous operation that checks the context
                        let! actualId =
                            queue.Run
                                setup
                                None
                                (fun () ->
                                    // Check context immediately
                                    let immediate = contexts.AsyncLocal.Value
                                    let (OutputStreamId immediateGuid) = immediate
                                    contextValues.Add (expectedId, immediateGuid)

                                    // Do some work that might cause context issues
                                    Thread.Sleep (10)

                                    // Check context after work
                                    let afterWork = contexts.AsyncLocal.Value
                                    let (OutputStreamId afterWorkGuid) = afterWork
                                    contextValues.Add (expectedId, afterWorkGuid)

                                    // Simulate calling into framework code that might use ExecutionContext
                                    let mutable capturedValue = Guid.Empty

                                    ExecutionContext.Run (
                                        ExecutionContext.Capture (),
                                        (fun _ ->
                                            let current = contexts.AsyncLocal.Value
                                            let (OutputStreamId currentGuid) = current
                                            capturedValue <- currentGuid
                                        ),
                                        ()
                                    )

                                    contextValues.Add (expectedId, capturedValue)

                                    afterWorkGuid
                                )

                        // Verify the returned value matches what we set
                        actualId |> shouldEqual expectedId
                    }
                )

            // Wait for all tasks
            let! results = Task.WhenAll (tasks)
            results |> Array.iter id

            // Verify all context values were correct
            let allValues = contextValues |> Seq.toList
            allValues |> shouldHaveLength 30 // 3 checks per operation * 10 operations

            // Every captured value should match its expected value
            for expected, actual in allValues do
                actual |> shouldEqual expected

            // Clean up
            let! _, teardown = queue.RunTestTearDown setup (fun () -> ())
            do! queue.EndTestFixture teardown
        }

    [<Test>]
    let ``ExecutionContext isolation between concurrent synchronous operations`` () =
        task {
            let dummyFixture =
                TestFixture.Empty typeof<obj> (Some (Parallelizable.Yes ClassParallelScope.All)) [] []

            use contexts = TestContexts.Empty ()
            use queue = new ParallelQueue (Some 4, None)

            let! running = queue.StartTestFixture dummyFixture
            let! _, setup = queue.RunTestSetup running (fun () -> ())

            // Use a barrier to ensure operations run concurrently
            let barrier = new Barrier (3)
            let seenValues = System.Collections.Concurrent.ConcurrentBag<int * Guid> ()

            // Create operations that will definitely run concurrently
            let tasks =
                [ 1..3 ]
                |> List.map (fun i ->
                    task {
                        // Each task sets its own context value
                        let myId = Guid.NewGuid ()
                        contexts.AsyncLocal.Value <- OutputStreamId myId

                        let! result =
                            queue.Run
                                setup
                                (Some (Parallelizable.Yes ()))
                                (fun () ->
                                    // Wait for all tasks to reach this point
                                    barrier.SignalAndWait ()

                                    // Now check what value we see
                                    let currentValue = contexts.AsyncLocal.Value

                                    match currentValue with
                                    | OutputStreamId guid -> seenValues.Add (i, guid)

                                    // Do some synchronous work
                                    Thread.Sleep (5)

                                    // Check again after work
                                    let afterWork = contexts.AsyncLocal.Value

                                    match afterWork with
                                    | OutputStreamId guid ->
                                        // Also verify we can write to the correct streams
                                        contexts.Stdout.WriteLine ($"Task {i} sees context {guid}")
                                        guid
                                )

                        // Each task should see its own value
                        result |> shouldEqual myId
                    }
                )

            let! results = Task.WhenAll (tasks)
            results |> Array.iter id

            // Verify we saw 3 different values (one per task)
            let values = seenValues |> Seq.toList
            values |> shouldHaveLength 3

            // All seen values should be different (no context bleeding)
            let uniqueValues = values |> List.map snd |> List.distinct
            uniqueValues |> shouldHaveLength 3

            let! _, teardown = queue.RunTestTearDown setup (fun () -> ())
            do! queue.EndTestFixture teardown
        }

    [<Test>]
    let ``ExecutionContext flows correctly through nested synchronous operations`` () =
        task {
            let dummyFixture =
                TestFixture.Empty typeof<obj> (Some (Parallelizable.Yes ClassParallelScope.All)) [] []

            use contexts = TestContexts.Empty ()
            use queue = new ParallelQueue (Some 4, None)

            let! running = queue.StartTestFixture dummyFixture
            let! _, setup = queue.RunTestSetup running (fun () -> ())

            // Set an initial context
            let outerGuid = Guid.NewGuid ()
            contexts.AsyncLocal.Value <- OutputStreamId outerGuid

            let! result =
                queue.Run
                    setup
                    None
                    (fun () ->
                        // Check we have the outer context
                        let outer = contexts.AsyncLocal.Value
                        let (OutputStreamId outerSeen) = outer
                        outerSeen |> shouldEqual outerGuid

                        // Now change the context for a nested operation
                        let innerGuid = Guid.NewGuid ()
                        contexts.AsyncLocal.Value <- OutputStreamId innerGuid

                        // Use Task.Run to potentially hop threads
                        let innerResult =
                            Task
                                .Run(fun () ->
                                    let inner = contexts.AsyncLocal.Value
                                    let (OutputStreamId innerSeen) = inner
                                    innerSeen |> shouldEqual innerGuid
                                    innerSeen
                                )
                                .Result

                        // After the nested operation, we should still have our inner context
                        let afterNested = contexts.AsyncLocal.Value
                        let (OutputStreamId afterNestedGuid) = afterNested
                        afterNestedGuid |> shouldEqual innerGuid

                        (outerSeen, innerResult, afterNestedGuid)
                    )

            // Unpack results
            let seenOuter, seenInner, seenAfter = result
            seenOuter |> shouldEqual outerGuid
            seenInner |> shouldNotEqual outerGuid
            seenAfter |> shouldEqual seenInner

            let! _, teardown = queue.RunTestTearDown setup (fun () -> ())
            do! queue.EndTestFixture teardown
        }

(*
    [<Test>]
    let ``ExecutionContext flows correctly through async operations`` () =
        task {
            // Create a test fixture
            let dummyFixture =
                TestFixture.Empty
                    typeof<obj>
                    (Some (Parallelizable.Yes ClassParallelScope.All))
                    []
                    []

            use contexts = TestContexts.Empty ()
            use queue = new ParallelQueue(Some 4, None)

            // Track which context values we see during execution
            let contextValues = System.Collections.Concurrent.ConcurrentBag<Guid * Guid>()

            // Start the fixture
            let! running = queue.StartTestFixture dummyFixture
            let! _, setup = queue.RunTestSetup running (fun () -> ())

            // Create several async operations with different context values
            let tasks =
                [1..10]
                |> List.map (fun i ->
                    task {
                        // Set a unique context value
                        let expectedId = Guid.NewGuid()
                        let outputId = OutputStreamId expectedId
                        contexts.AsyncLocal.Value <- outputId

                        // Run an async operation that checks the context at multiple points
                        let! actualId =
                            queue.RunAsync setup None (fun () ->
                                async {
                                    // Check context immediately
                                    let immediate = contexts.AsyncLocal.Value
                                    let (OutputStreamId immediateGuid) = immediate
                                    contextValues.Add(expectedId, immediateGuid)

                                    // Yield to allow potential context loss
                                    do! Async.Sleep 10

                                    // Check context after yield
                                    let afterYield = contexts.AsyncLocal.Value
                                    let (OutputStreamId afterYieldGuid) = afterYield
                                    contextValues.Add(expectedId, afterYieldGuid)

                                    // Do some actual async work
                                    do! Task.Delay(10) |> Async.AwaitTask

                                    // Check context after task
                                    let afterTask = contexts.AsyncLocal.Value
                                    let (OutputStreamId afterTaskGuid) = afterTask
                                    contextValues.Add(expectedId, afterTaskGuid)

                                    return afterTaskGuid
                                }
                            )

                        // Verify the returned value matches what we set
                        actualId |> shouldEqual expectedId
                    }
                )

            // Wait for all tasks
            let! results = Task.WhenAll(tasks)
            results |> Array.iter id

            // Verify all context values were correct
            let allValues = contextValues |> Seq.toList
            allValues |> shouldHaveLength 30  // 3 checks per operation * 10 operations

            // Every captured value should match its expected value
            for expected, actual in allValues do
                actual |> shouldEqual expected

            // Clean up
            let! _, teardown = queue.RunTestTearDown setup (fun () -> ())
            do! queue.EndTestFixture teardown
        }

    [<Test>]
    let ``ExecutionContext isolation between concurrent operations`` () =
        task {
            let dummyFixture =
                TestFixture.Empty
                    typeof<obj>
                    (Some (Parallelizable.Yes ClassParallelScope.All))
                    []
                    []

            use contexts = TestContexts.Empty ()
            use queue = new ParallelQueue(Some 4, None)

            let! running = queue.StartTestFixture dummyFixture
            let! _, setup = queue.RunTestSetup running (fun () -> ())

            // Use a barrier to ensure operations run concurrently
            let barrier = new Barrier(3)
            let seenValues = System.Collections.Concurrent.ConcurrentBag<int * Guid option>()

            // Create operations that will definitely run concurrently
            let tasks =
                [1..3]
                |> List.map (fun i ->
                    task {
                        // Each task sets its own context value
                        let myId = Guid.NewGuid()
                        contexts.AsyncLocal.Value <- OutputStreamId myId

                        let! result =
                            queue.RunAsync setup (Some (Parallelizable.Yes ())) (fun () ->
                                async {
                                    // Wait for all tasks to reach this point
                                    barrier.SignalAndWait() |> ignore

                                    // Now check what value we see
                                    let currentValue = contexts.AsyncLocal.Value
                                    match currentValue with
                                    | OutputStreamId guid -> seenValues.Add(i, Some guid)
                                    | _ -> seenValues.Add(i, None)

                                    // Do some async work
                                    do! Async.Sleep 5

                                    // Check again after async work
                                    let afterAsync = contexts.AsyncLocal.Value
                                    match afterAsync with
                                    | OutputStreamId guid ->
                                        return guid
                                    | _ ->
                                        return failwith "Lost context after async"
                                }
                            )

                        // Each task should see its own value
                        result |> shouldEqual myId
                    }
                )

            let! results = Task.WhenAll(tasks)
            results |> Array.iter id

            // Verify we saw 3 different values (one per task)
            let values = seenValues |> Seq.toList
            values |> shouldHaveLength 3

            // Each task should have seen a value
            for (taskId, value) in values do
                value |> shouldNotEqual None

            // All seen values should be different (no context bleeding)
            let uniqueValues =
                values
                |> List.choose snd
                |> List.distinct
            uniqueValues |> shouldHaveLength 3

            let! _, teardown = queue.RunTestTearDown setup (fun () -> ())
            do! queue.EndTestFixture teardown
        }
*)
