namespace FailingConsumer

open NUnit.Framework

[<TestFixture>]
module TestInsufficientArgs =

    [<Test>]
    let foo (_ : int) = ()
