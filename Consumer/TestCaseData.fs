namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestCaseData =
    let testCasesSeen = ResizeArray ()

    let dataSourceRaw = [ 3, "hi", [| 4.0 |] ; -10, "bye", null ]

    [<OneTimeTearDown>]
    let tearDown () =
        testCasesSeen
        |> Seq.toList
        |> List.sortBy (fun (a, _, _) -> a)
        |> shouldEqual (dataSourceRaw |> List.sortBy (fun (a, _, _) -> a))

    let dataSource = dataSourceRaw |> List.map TestCaseData

    [<TestCaseSource(nameof dataSource)>]
    let ``Consume test data`` (i : int, s : string, arr : float[]) =
        lock testCasesSeen (fun () -> testCasesSeen.Add (i, s, arr))
