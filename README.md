# Toy NUnit test runner

## Filtering

To supply special characters in a string, XML-encode them and `"quote"` the string; if you give a quoted string, we will XML-decode the string.
(In an unquoted string, we will just do our best; special characters may or may not result in parse failures and unexpected parses.)

We support at least the [documented `dotnet test` examples](https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests).
However, we would recommend phrasing some of them differently, for maximum peace of mind:
* `FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod`. This would be better phrased with quotes and escaping as `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`
