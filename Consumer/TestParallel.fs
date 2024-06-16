namespace Consumer

open System
open System.Threading
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
[<Parallelizable>]
module TestParallelDefault =

    let defaults = List.init 60 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing, no scope`` (i : int) =
        Console.WriteLine i
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestParallelAllScope =

    let defaults = List.init 60 id

    [<TestCaseSource(nameof defaults)>]
    let ``Thing, all scope`` (i : int) =
        Console.WriteLine i
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Self)>]
module TestParallelSelfScope =

    let defaults = List.init 60 id

    [<TestCaseSource(nameof defaults)>]
    let ``Thing, self scope`` (i : int) =
        Console.WriteLine i
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Children)>]
module TestParallelChildrenScope =

    let defaults = List.init 60 id

    [<TestCaseSource(nameof defaults)>]
    let ``Thing, children scope`` (i : int) =
        Console.WriteLine i
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Fixtures)>]
module TestParallelFixturesScope =

    let defaults = List.init 60 id

    [<TestCaseSource(nameof defaults)>]
    let ``Thing, fixtures scope`` (i : int) =
        Console.WriteLine i
        Thread.Sleep (TimeSpan.FromMilliseconds (float i))
        i |> shouldEqual i
