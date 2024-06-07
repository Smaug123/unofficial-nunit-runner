namespace Consumer

open NUnit.Framework

[<TestFixture>]
module Inconclusive =

    [<Test>]
    let ``Inconclusive test`` () =
        Assert.Inconclusive "I was inconclusive"

    [<Test>]
    let ``Ignore test`` () = Assert.Ignore "I am ignored"

    [<Test>]
    let ``Pass test`` () = Assert.Pass "ohhhh yeaahhhh"

    [<Test>]
    let ``Warning`` () = Assert.Warn "warning"
