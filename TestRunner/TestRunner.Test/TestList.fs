namespace TestRunner.Test

open FsCheck
open FsUnitTyped
open TestRunner
open NUnit.Framework

[<TestFixture>]
module TestList =

    [<Test>]
    let ``combinations has right size`` () =
        let property (xs : int list list) =
            let combs = List.combinations xs

            combs.Length
            |> shouldEqual ((1, xs) ||> List.fold (fun acc l -> acc * l.Length))

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``each combination is drawn from the right set`` () =
        let property (xs : int list list) =
            let combs = List.combinations xs

            for comb in combs do
                comb.Length |> shouldEqual xs.Length

                for i = 0 to comb.Length - 1 do
                    xs.[i] |> shouldContain comb.[i]

        Check.QuickThrowOnFailure property
