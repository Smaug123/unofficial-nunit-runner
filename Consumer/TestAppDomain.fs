namespace Consumer

open System
open FsUnitTyped
open System.IO
open NUnit.Framework

[<TestFixture>]
module TestAppDomain =

    [<Test>]
    let ``Can load config from app domain`` () =
        Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "some-config.json")
        |> File.ReadAllText
        |> fun s -> s.Trim ()
        |> shouldEqual """{"hi":"bye"}"""

    [<Test>]
    let ``Can load config from working dir`` () =
        "some-config.json"
        |> File.ReadAllText
        |> fun s -> s.Trim ()
        |> shouldEqual """{"hi":"bye"}"""
