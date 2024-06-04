namespace TestRunner.Test

open TestRunner
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestFilter =

    let docExamples =
        [
            "(Name~MyClass) | (Name~MyClass2)",
            FilterIntermediate.Or (
                FilterIntermediate.Contains (FilterIntermediate.Name, FilterIntermediate.String "MyClass"),
                FilterIntermediate.Contains (FilterIntermediate.Name, FilterIntermediate.String "MyClass2")
            )
            "xyz", FilterIntermediate.Contains (FilterIntermediate.FullyQualifiedName, FilterIntermediate.String "xyz")
            "FullyQualifiedName~xyz",
            FilterIntermediate.Contains (FilterIntermediate.FullyQualifiedName, FilterIntermediate.String "xyz")
            "FullyQualifiedName!~IntegrationTests",
            FilterIntermediate.Not (
                FilterIntermediate.Contains (
                    FilterIntermediate.FullyQualifiedName,
                    FilterIntermediate.String "IntegrationTests"
                )
            )
            "FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod",
            FilterIntermediate.Equal (
                FilterIntermediate.FullyQualifiedName,
                FilterIntermediate.String "MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod"
            )
            "Name~Method", FilterIntermediate.Contains (FilterIntermediate.Name, FilterIntermediate.String "Method")
            "FullyQualifiedName!=MSTestNamespace.UnitTest1.TestMethod1",
            FilterIntermediate.Not (
                FilterIntermediate.Equal (
                    FilterIntermediate.FullyQualifiedName,
                    FilterIntermediate.String "MSTestNamespace.UnitTest1.TestMethod1"
                )
            )
            "TestCategory=CategoryA",
            FilterIntermediate.Equal (FilterIntermediate.TestCategory, FilterIntermediate.String "CategoryA")
            "FullyQualifiedName~UnitTest1|TestCategory=CategoryA",
            FilterIntermediate.Or (
                FilterIntermediate.Contains (
                    FilterIntermediate.FullyQualifiedName,
                    FilterIntermediate.String "UnitTest1"
                ),
                FilterIntermediate.Equal (FilterIntermediate.TestCategory, FilterIntermediate.String "CategoryA")
            )
            "FullyQualifiedName~UnitTest1&TestCategory=CategoryA",
            FilterIntermediate.And (
                FilterIntermediate.Contains (
                    FilterIntermediate.FullyQualifiedName,
                    FilterIntermediate.String "UnitTest1"
                ),
                FilterIntermediate.Equal (FilterIntermediate.TestCategory, FilterIntermediate.String "CategoryA")
            )
            "(FullyQualifiedName~UnitTest1&TestCategory=CategoryA)|TestCategory=1",
            FilterIntermediate.Or (
                FilterIntermediate.And (
                    FilterIntermediate.Contains (
                        FilterIntermediate.FullyQualifiedName,
                        FilterIntermediate.String "UnitTest1"
                    ),
                    FilterIntermediate.Equal (FilterIntermediate.TestCategory, FilterIntermediate.String "CategoryA")
                ),
                FilterIntermediate.Equal (FilterIntermediate.TestCategory, FilterIntermediate.String "1")
            )
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof docExamples)>]
    let ``Doc examples`` (example : string, expected : FilterIntermediate) =
        FilterIntermediate.parse example |> shouldEqual expected

    let docExamplesRefined =
        [
            "(Name~MyClass) | (Name~MyClass2)",
            Filter.Or (Filter.Name (Match.Contains "MyClass"), Filter.Name (Match.Contains "MyClass2"))
            "xyz", Filter.FullyQualifiedName (Match.Contains "xyz")
            "FullyQualifiedName~xyz", Filter.FullyQualifiedName (Match.Contains "xyz")
            "FullyQualifiedName!~IntegrationTests",
            Filter.Not (Filter.FullyQualifiedName (Match.Contains "IntegrationTests"))
            "FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod",
            Filter.FullyQualifiedName (
                Match.Exact "MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod"
            )
            "Name~Method", Filter.Name (Match.Contains "Method")
            "FullyQualifiedName!=MSTestNamespace.UnitTest1.TestMethod1",
            Filter.Not (Filter.FullyQualifiedName (Match.Exact "MSTestNamespace.UnitTest1.TestMethod1"))
            "TestCategory=CategoryA", Filter.TestCategory (Match.Exact "CategoryA")
            "FullyQualifiedName~UnitTest1|TestCategory=CategoryA",
            Filter.Or (
                Filter.FullyQualifiedName (Match.Contains "UnitTest1"),
                Filter.TestCategory (Match.Exact "CategoryA")
            )
            "FullyQualifiedName~UnitTest1&TestCategory=CategoryA",
            Filter.And (
                Filter.FullyQualifiedName (Match.Contains "UnitTest1"),
                Filter.TestCategory (Match.Exact "CategoryA")
            )
            "(FullyQualifiedName~UnitTest1&TestCategory=CategoryA)|TestCategory=1",
            Filter.Or (
                Filter.And (
                    Filter.FullyQualifiedName (Match.Contains "UnitTest1"),
                    Filter.TestCategory (Match.Exact "CategoryA")
                ),
                Filter.TestCategory (Match.Exact "1")
            )
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof docExamplesRefined)>]
    let ``Doc examples, refined`` (example : string, expected : Filter) =
        FilterIntermediate.parse example |> Filter.make |> shouldEqual expected
