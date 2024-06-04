namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestValues =

    let seen = ResizeArray ()

    [<Test>]
    let ``Can consume values`` ([<Values(true, false)>] x : bool) : unit = lock seen (fun () -> seen.Add x)

    [<OneTimeTearDown>]
    let ``Values are all OK`` () =
        seen |> Seq.toList |> shouldEqual [ true ; false ]
