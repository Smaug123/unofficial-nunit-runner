namespace TestRunner.Test

open TestRunner
open NUnit.Framework

[<TestFixture>]
module TestTrx =

    [<Test>]
    let ``Can parse the first example`` () =
        let resource = EmbeddedResource.read "Example1.trx"

        let parsed =
            match TrxReport.parse resource with
            | Error e -> failwith $"Expected successful parse: %s{e}"
            | Ok r -> r

        ()
