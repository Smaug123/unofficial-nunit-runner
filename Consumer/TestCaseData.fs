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

    let optionalData = [ Some "hi" ; None ] |> List.map TestCaseData

    [<TestCaseSource(nameof optionalData)>]
    let ``Consume options, TestCaseData`` (s : string option) : unit = s |> shouldEqual s

    let optionalRaw = [ Some "hi" ; None ]

    [<TestCaseSource(nameof optionalRaw)>]
    let ``Consume options, raw`` (s : string option) : unit = s |> shouldEqual s

    [<TestCase(30, 15, 44, false)>]
    let bug66 (i : int, j : int, k : int, l : bool) =
        i |> shouldEqual 30
        j |> shouldEqual 15
        k |> shouldEqual 44
        l |> shouldEqual false

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
