# Toy NUnit test runner

## Filtering

To supply special characters in a string, XML-encode them and `"quote"` the string; if you give a quoted string, we will XML-decode the string.
(In an unquoted string, we will just do our best; special characters may or may not result in parse failures and unexpected parses.)

We support at least the [documented `dotnet test` examples](https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests).
However, we would recommend phrasing some of them differently, for maximum peace of mind:
* `FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod`. This would be better phrased with quotes and escaping as `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`

## Parallelism

WoofWare.NUnitTestRunner has *limited* support for parallelism.
By default, we run tests in parallel, taking half the available processors; we may or may not respect the NUnit parallelism attributes to any given extent that they tell us to be *more* parallel (but we will never incorrectly run tests in parallel).

# Licence

WoofWare.NUnitTestRunner is licensed to you under the MIT licence, a copy of which can be found at [LICENSE](./LICENSE).
