namespace Consumer

open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
[<Parallelizable>]
module TestParallelDefault =

    let defaults = List.init 100 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing`` (i : int) = i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestParallelAllScope =

    let defaults = List.init 100 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing`` (i : int) = i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Self)>]
module TestParallelSelfScope =

    let defaults = List.init 100 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing`` (i : int) = i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Children)>]
module TestParallelChildrenScope =

    let defaults = List.init 100 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing`` (i : int) = i |> shouldEqual i

[<TestFixture>]
[<Parallelizable(ParallelScope.Fixtures)>]
module TestParallelFixturesScope =

    let defaults = List.init 100 id

    [<TestCaseSource(nameof defaults)>]
    let ``Default thing`` (i : int) = i |> shouldEqual i
