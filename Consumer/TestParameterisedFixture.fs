namespace Consumer

open NUnit.Framework
open FsUnitTyped

[<TestFixture true>]
[<TestFixture false>]
type TestParameterisedFixture (v : bool) =
    [<Test>]
    member _.Thing () = v |> shouldEqual v

[<TestFixture(3, true)>]
[<TestFixture(6, false)>]
type TestParameterisedFixtureMultiple (i : int, v : bool) =
    [<Test>]
    member _.Thing () =
        v |> shouldEqual v
        i |> shouldEqual i

[<TestFixture>]
module TestThing =
    type DU = | Foo of int

    [<Test>]
    let foo (i : DU) = ()
