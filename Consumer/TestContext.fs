namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestContext =

    [<TestCase 3>]
    let ``Context has appropriate values`` (i : int) =
        // We explicitly cannot support this (https://github.com/dotnet/dotnet-api-docs/pull/3869/files).
        // TestContext.Progress.WriteLine "hi!"

        TestContext.CurrentContext.Test.MethodName
        |> shouldEqual "Context has appropriate values"

        TestContext.CurrentContext.Test.Name
        |> shouldEqual "Context has appropriate values(3)"

        TestContext.CurrentContext.Test.Namespace |> shouldEqual "Consumer"
        TestContext.CurrentContext.Test.ClassName |> shouldEqual "Consumer.TestContext"

        TestContext.CurrentContext.Test.FullName
        |> shouldEqual "Consumer.TestContext.Context has appropriate values(3)"

        i |> shouldEqual 3
        TestContext.CurrentContext.Test.Arguments |> List.ofArray |> shouldEqual [ 3 ]
