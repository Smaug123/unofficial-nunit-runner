namespace Consumer

open System.Threading
open FsUnitTyped
open NUnit.Framework

[<RequireQualifiedAccess>]
module NonStaticTestHelpers =
    let count = ref 0

[<TestFixture>]
type TestNonStatic () =
    let count = ref 0

    member this.Thing = "i'm a thing"

    [<Test>]
    member this.Foo () =
        Interlocked.Increment NonStaticTestHelpers.count |> ignore<int>
        Interlocked.Increment count |> ignore<int>
        this.Thing |> shouldEqual "i'm a thing"

    [<Test>]
    static member AnotherTest () =
        Interlocked.Increment NonStaticTestHelpers.count |> ignore<int>
        1 |> shouldEqual 1

    [<OneTimeTearDown>]
    member _.TearDown () =
        count.Value |> shouldEqual 1
        NonStaticTestHelpers.count.Value |> shouldEqual 2
