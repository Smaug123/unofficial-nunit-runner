namespace Consumer

open NUnit.Framework
open FsUnitTyped

module NoAttribute =

    [<Test>]
    let foo () = 1 |> shouldEqual 1
