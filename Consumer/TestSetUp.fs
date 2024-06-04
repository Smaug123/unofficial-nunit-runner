namespace Consumer

open FsUnitTyped
open System.Threading
open NUnit.Framework

[<TestFixture>]
module TestSetUp =

    let haveOneTimeSetUp = ref 0

    [<OneTimeSetUp>]
    let oneTimeSetUp () =
        if Interlocked.Increment haveOneTimeSetUp <> 1 then
            failwith "one time setup happened more than once"

    let setUpTimes = ref 0
    let tearDownTimes = ref 0

    let setUpTimesSeen = ResizeArray<int> ()
    let tearDownTimesSeen = ResizeArray<int> ()

    [<SetUp>]
    let setUp () =
        haveOneTimeSetUp.Value |> shouldEqual 1
        let newId = Interlocked.Increment setUpTimes
        lock setUpTimesSeen (fun () -> setUpTimesSeen.Add newId)

    [<TearDown>]
    let tearDown () =
        let newId = Interlocked.Increment tearDownTimes
        lock tearDownTimesSeen (fun () -> tearDownTimesSeen.Add newId)

    let haveOneTimeTearDown = ref 0

    [<OneTimeTearDown>]
    let oneTimeTearDown () =
        if Interlocked.Increment haveOneTimeTearDown <> 1 then
            failwith "one time tear down happened more than once"

        setUpTimesSeen
        |> Seq.toList
        // Six tests: one for Test, two for the TestCase, three for the Repeat.
        |> shouldEqual [ 1..6 ]

        tearDownTimesSeen |> Seq.toList |> shouldEqual [ 1..6 ]

    [<Test>]
    let ``Test 1`` () =
        haveOneTimeTearDown.Value |> shouldEqual 0
        1 |> shouldEqual 1

    [<TestCase "h">]
    [<TestCase "i">]
    let ``Test 2`` (s : string) =
        haveOneTimeTearDown.Value |> shouldEqual 0
        s.Length |> shouldEqual 1

    [<Test>]
    [<Repeat 3>]
    let ``Test 3`` () =
        haveOneTimeTearDown.Value |> shouldEqual 0
        1 |> shouldEqual 1
