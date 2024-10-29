namespace Consumer

open System
open System.Collections.Concurrent
open System.Threading
open NUnit.Framework
open FsUnitTyped

// These tests are flaky if the bug https://github.com/Smaug123/unofficial-nunit-runner/issues/168 is unfixed.
[<TestFixture>]
module TestParallelIndividualTest =

    type private Transitions =
        | Started of int
        | LockAcquired of int
        | Exited of int

    let locker = obj ()
    let private sequence = ConcurrentQueue<Transitions> ()

    [<Test>]
    [<Parallelizable(ParallelScope.None)>]
    let ``does not run in parallel`` () =
        sequence.Enqueue (Transitions.Started 0)
        let entered = Monitor.TryEnter (locker, TimeSpan.Zero)

        if entered then
            sequence.Enqueue (Transitions.LockAcquired 0)
            Monitor.Exit locker
            sequence.Enqueue (Transitions.Exited 0)
        else
            sequence.Enqueue (Transitions.Exited 0)
            failwith "failed to acquire the lock"

    [<Test>]
    let ``unrestricted parallelism`` () =
        sequence.Enqueue (Transitions.Started 1)
        let entered = Monitor.TryEnter (locker, TimeSpan.Zero)

        if entered then
            sequence.Enqueue (Transitions.LockAcquired 1)
            Monitor.Exit locker
            sequence.Enqueue (Transitions.Exited 1)
        else
            sequence.Enqueue (Transitions.Exited 1)
            failwith "failed to acquire the lock"

    [<OneTimeTearDown>]
    let ``It worked`` () =
        let sequence = sequence |> Seq.toList

        let allowed n =
            [ Transitions.Started n ; Transitions.LockAcquired n ; Transitions.Exited n ]

        if sequence <> allowed 0 @ allowed 1 && sequence <> allowed 1 @ allowed 0 then
            let s = sequence |> Seq.map string<Transitions> |> String.concat "\n"
            failwith $"Unexpected sequence!\n%s{s}"

        ()
