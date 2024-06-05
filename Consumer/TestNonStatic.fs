namespace Consumer

open System.Threading
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
type TestNonStatic () =
    let count = ref 0

    member this.Thing = "i'm a thing"

    [<Test>]
    member this.Foo () =
        Interlocked.Increment count |> ignore<int>
        this.Thing |> shouldEqual "I'm a thing"

    [<OneTimeTearDown>]
    member _.TearDown () = count.Value |> shouldEqual 1
