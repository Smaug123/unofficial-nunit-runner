namespace Consumer

open NUnit.Framework

[<TestFixture>]
module TestExplicitIndividual =

    [<Explicit>]
    [<Test>]
    let ``This test should not be run`` () = failwith<unit> "should not call"

[<Explicit>]
[<TestFixture>]
module TestExplicitModule =

    [<OneTimeSetUp>]
    let setUp () = failwith<unit> "should not call: setup"

    [<OneTimeTearDown>]
    let tearDown () =
        failwith<unit> "should not call: teardown"

    [<Test>]
    let ``This test should not be run because its module is explicit`` () = failwith<unit> "should not call: test"
