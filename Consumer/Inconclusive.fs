namespace Consumer

open NUnit.Framework

[<TestFixture>]
module Inconclusive =

    [<Test>]
    let ``Inconclusive test`` () = Assert.Inconclusive ()

    [<Test>]
    let ``Ignore test`` () = Assert.Ignore ()

    [<Test>]
    let ``Pass test`` () = Assert.Pass ()

    [<Test>]
    let ``Warning`` () = Assert.Warn "warning"
