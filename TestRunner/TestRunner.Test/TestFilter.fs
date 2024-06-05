namespace TestRunner.Test

open TestRunner
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestFilter =

    let docExamples =
        [
            "(Name~MyClass) | (Name~MyClass2)",
            ParsedFilter.Or (
                ParsedFilter.Contains (ParsedFilter.Name, ParsedFilter.String "MyClass"),
                ParsedFilter.Contains (ParsedFilter.Name, ParsedFilter.String "MyClass2")
            )
            "xyz", ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "xyz")
            "FullyQualifiedName~xyz", ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "xyz")
            "FullyQualifiedName!~IntegrationTests",
            ParsedFilter.Not (
                ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "IntegrationTests")
            )
            "FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod",
            ParsedFilter.Equal (
                ParsedFilter.FullyQualifiedName,
                ParsedFilter.String "MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod"
            )
            "Name~Method", ParsedFilter.Contains (ParsedFilter.Name, ParsedFilter.String "Method")
            "FullyQualifiedName!=MSTestNamespace.UnitTest1.TestMethod1",
            ParsedFilter.Not (
                ParsedFilter.Equal (
                    ParsedFilter.FullyQualifiedName,
                    ParsedFilter.String "MSTestNamespace.UnitTest1.TestMethod1"
                )
            )
            "TestCategory=CategoryA", ParsedFilter.Equal (ParsedFilter.TestCategory, ParsedFilter.String "CategoryA")
            "FullyQualifiedName~UnitTest1|TestCategory=CategoryA",
            ParsedFilter.Or (
                ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "UnitTest1"),
                ParsedFilter.Equal (ParsedFilter.TestCategory, ParsedFilter.String "CategoryA")
            )
            "FullyQualifiedName~UnitTest1&TestCategory=CategoryA",
            ParsedFilter.And (
                ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "UnitTest1"),
                ParsedFilter.Equal (ParsedFilter.TestCategory, ParsedFilter.String "CategoryA")
            )
            "(FullyQualifiedName~UnitTest1&TestCategory=CategoryA)|TestCategory=1",
            ParsedFilter.Or (
                ParsedFilter.And (
                    ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, ParsedFilter.String "UnitTest1"),
                    ParsedFilter.Equal (ParsedFilter.TestCategory, ParsedFilter.String "CategoryA")
                ),
                ParsedFilter.Equal (ParsedFilter.TestCategory, ParsedFilter.String "1")
            )
        ]
        |> List.map TestCaseData

    // sigh, NUnit doesn't want to run internal tests
    [<TestCaseSource(nameof docExamples)>]
    let ``Doc examples`` (example : string, expected : obj) =
        let expected = expected |> unbox<ParsedFilter>
        ParsedFilter.parse example |> shouldEqual expected

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
        Filter.parse example |> shouldEqual expected
