namespace Consumer

open System
open System.Threading
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestAsyncOneTimeSetUp =

    let setUpRunCount = ref 0
    let tearDownRunCount = ref 0

    [<OneTimeSetUp>]
    let oneTimeSetUp () : Async<unit> =
        async {
            do! Async.Sleep (TimeSpan.FromMilliseconds 10.0)
            Interlocked.Increment setUpRunCount |> ignore<int>
        }

    [<OneTimeTearDown>]
    let oneTimeTearDown () : Async<unit> =
        async {
            do! Async.Sleep (TimeSpan.FromMilliseconds 10.0)

            if Interlocked.Increment tearDownRunCount <> 1 then
                failwith "one-time tear-down ran more than once"
        }

    [<Test>]
    let ``async one-time setup has completed before tests run`` () =
        setUpRunCount.Value |> shouldEqual 1
        tearDownRunCount.Value |> shouldEqual 0

[<TestFixture>]
module TestTaskOneTimeSetUp =

    let setUpRunCount = ref 0
    let tearDownRunCount = ref 0

    [<OneTimeSetUp>]
    let oneTimeSetUp () : Task =
        task {
            do! Task.Delay (TimeSpan.FromMilliseconds 10.0)
            Interlocked.Increment setUpRunCount |> ignore<int>
        }

    [<OneTimeTearDown>]
    let oneTimeTearDown () : Task =
        task {
            do! Task.Delay (TimeSpan.FromMilliseconds 10.0)

            if Interlocked.Increment tearDownRunCount <> 1 then
                failwith "one-time tear-down ran more than once"
        }

    [<Test>]
    let ``task one-time setup has completed before tests run`` () =
        setUpRunCount.Value |> shouldEqual 1
        tearDownRunCount.Value |> shouldEqual 0
