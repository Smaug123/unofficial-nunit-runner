namespace Consumer

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestContext =

    [<TestCase 3>]
    let ``Context has appropriate values`` (_ : int) =
        TestContext.Progress.WriteLine "hi!"

        TestContext.CurrentContext.Test.MethodName
        |> shouldEqual "Context has appropriate values"

        TestContext.CurrentContext.Test.Name
        |> shouldEqual "Context has appropriate values(3)"

        TestContext.CurrentContext.Test.Namespace |> shouldEqual "Consumer"
        TestContext.CurrentContext.Test.ClassName |> shouldEqual "Consumer.TestContext"

        TestContext.CurrentContext.Test.FullName
        |> shouldEqual "Consumer.TestContext.Context has appropriate values(3)"

        TestContext.CurrentContext.Test.Arguments |> List.ofArray |> shouldEqual [ 3 ]
