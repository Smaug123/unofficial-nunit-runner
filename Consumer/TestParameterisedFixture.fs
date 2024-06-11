namespace Consumer

open NUnit.Framework
open FsUnitTyped

[<TestFixture true>]
[<TestFixture false>]
type TestParameterisedFixture (v : bool) =
    [<Test>]
    let thing () = v |> shouldEqual v
