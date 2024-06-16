namespace Consumer

open System
open System.Threading
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
[<NonParallelizable>]
module TestNonParallel =
    let defaults = List.init 40 id
    let lock = ref 0

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing, but not parallel`` (i : int) =
        Interlocked.Increment lock |> shouldEqual 1
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        lock.Value <- 0
        i |> shouldEqual i
