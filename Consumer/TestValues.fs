namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestValues =

    let seen1 = ResizeArray<bool> ()

    [<Test>]
    let ``Can consume values, single boolean`` ([<Values(true, false)>] x : bool) : unit =
        lock seen1 (fun () -> seen1.Add x)

    let seen2 = ResizeArray ()

    [<Test ; Sequential>]
    let ``Can consume values, two bools, sequential, lengths match``
        ([<Values(true, false)>] x : bool, [<Values(false, true)>] y)
        : unit
        =
        lock seen2 (fun () -> seen2.Add (x, y))

    let seen3 = ResizeArray<int * obj> ()

    [<Test ; Sequential>]
    let ``Can consume values, two ints, sequential, lengths don't match, value type``
        ([<Values(88, 31)>] x : int, [<Values 29>] y : int)
        : unit
        =
        lock seen3 (fun () -> seen3.Add (x, box y))

    let seen4 = ResizeArray<string * obj> ()

    [<Test ; Sequential>]
    let ``Can consume values, two strings, sequential, lengths don't match, reference type``
        ([<Values("hi", "bye")>] x : string, [<Values "ohh">] y : string)
        : unit
        =
        lock seen4 (fun () -> seen4.Add (x, box y))

    let seen5 = ResizeArray<int * obj> ()

    [<Test ; Combinatorial>]
    let ``Can consume values, two ints, combinatorial, lengths don't match, value type``
        ([<Values(88, 31)>] x : int, [<Values 29>] y : int)
        : unit
        =
        lock seen5 (fun () -> seen5.Add (x, box y))

    let seen6 = ResizeArray<string * obj> ()

    [<Test ; Combinatorial>]
    let ``Can consume values, two strings, combinatorial, lengths don't match, reference type``
        ([<Values("hi", "bye")>] x : string, [<Values "ohh">] y : string)
        : unit
        =
        lock seen6 (fun () -> seen6.Add (x, box y))

    let seen7 = ResizeArray<int * obj> ()

    [<Test>]
    let ``Can consume values, two ints, implicit combinatorial, lengths don't match, value type``
        ([<Values(88, 31)>] x : int, [<Values 29>] y : int)
        : unit
        =
        lock seen7 (fun () -> seen7.Add (x, box y))

    let seen8 = ResizeArray<string * obj> ()

    [<Test>]
    let ``Can consume values, two strings, implicit combinatorial, lengths don't match, reference type``
        ([<Values("hi", "bye")>] x : string, [<Values "ohh">] y : string)
        : unit
        =
        lock seen8 (fun () -> seen8.Add (x, box y))

    let seen9 = ResizeArray<string * string> ()

    [<Test>]
    let ``Can consume values, two strings, implicit combinatorial, reference type``
        ([<Values("hi", "bye", "whoa")>] x : string, [<Values("x1", "x2")>] y : string)
        : unit
        =
        lock seen9 (fun () -> seen9.Add (x, y))

    [<OneTimeTearDown>]
    let ``Values are all OK`` () =
        seen1 |> Seq.toList |> shouldEqual [ true ; false ]
        seen2 |> Seq.toList |> shouldEqual [ (true, false) ; (false, true) ]
        seen3 |> Seq.toList |> shouldEqual [ (88, 29) ; (31, 0) ]
        seen4 |> Seq.toList |> shouldEqual [ ("hi", "ohh") ; ("bye", null) ]
        seen5 |> Seq.toList |> shouldEqual [ (88, 29) ; (31, 29) ]
        seen6 |> Seq.toList |> shouldEqual [ ("hi", "ohh") ; ("bye", "ohh") ]
        seen7 |> Seq.toList |> shouldEqual [ (88, 29) ; (31, 29) ]
        seen8 |> Seq.toList |> shouldEqual [ ("hi", "ohh") ; ("bye", "ohh") ]

        seen9
        |> Seq.toList
        |> List.sort
        |> shouldEqual (
            List.sort
                [
                    ("hi", "x1")
                    ("bye", "x1")
                    ("whoa", "x1")
                    ("hi", "x2")
                    ("bye", "x2")
                    ("whoa", "x2")
                ]
        )
