namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestCaseData =
    let testCasesSeen = ResizeArray ()

    let dataSourceRaw = [ 3, "hi", [| 4.0 |] ; -10, "bye", null ]

    let dataSource = dataSourceRaw |> List.map TestCaseData

    [<TestCaseSource(nameof dataSource)>]
    let ``Consume test data`` (i : int, s : string, arr : float[]) =
        lock testCasesSeen (fun () -> testCasesSeen.Add (i, s, arr))

    let multipleSources = ResizeArray ()

    let dataSource2Raw = [ 5, "egg", [| -1.3 ; 4.5 |] ; 100, "mycroft", [||] ]
    let dataSource2 = dataSource2Raw |> List.map TestCaseData

    [<TestCaseSource(nameof dataSource)>]
    [<TestCaseSource(nameof dataSource2)>]
    let ``Consume test data from multiple sources`` (i : int, s : string, arr : float[]) =
        lock multipleSources (fun () -> multipleSources.Add (i, s, arr))

    let optional = [ Some "hi" ; None ] |> List.map TestCaseData

    [<TestCaseSource(nameof optional)>]
    let ``Consume options`` (s : string option) : unit = s |> shouldEqual s

    [<OneTimeTearDown>]
    let tearDown () =
        testCasesSeen
        |> Seq.toList
        |> List.sortBy (fun (a, _, _) -> a)
        |> shouldEqual (dataSourceRaw |> List.sortBy (fun (a, _, _) -> a))

        multipleSources
        |> Seq.toList
        |> List.sortBy (fun (a, _, _) -> a)
        |> shouldEqual ((dataSourceRaw @ dataSource2Raw) |> List.sortBy (fun (a, _, _) -> a))
