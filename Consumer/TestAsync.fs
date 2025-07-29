namespace Consumer

open System
open System.Threading.Tasks
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestAsync =

    [<Test>]
    let ``an async test`` () =
        async {
            do! Async.Sleep (TimeSpan.FromMilliseconds 20.0)
            1 |> shouldEqual 2
        }

    [<Test>]
    let ``an async test, task-based`` () =
        task {
            do! Task.Delay (TimeSpan.FromMilliseconds 20.0)
            1 |> shouldEqual 2
        }
