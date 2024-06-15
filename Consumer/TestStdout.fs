namespace Consumer

open System
open NUnit.Framework

[<TestFixture>]
module TestStdout =

    [<Test>]
    let ``Stdout is redirected`` () =
        Console.Out.WriteLine "Hi!"
        Console.WriteLine "Hi! part 2"
        Console.Error.WriteLine "Bye!"
