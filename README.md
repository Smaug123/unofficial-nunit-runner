# Toy NUnit test runner

## Filtering

To supply special characters in a string, XML-encode them and `"quote"` the string; if you give a quoted string, we will XML-decode the string.
(In an unquoted string, we will just do our best; special characters may or may not result in parse failures and unexpected parses.)

We support at least the [documented `dotnet test` examples](https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests).
However, we would recommend phrasing some of them differently, for maximum peace of mind:
* `FullyQualifiedName=MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod`. This would be better phrased with quotes and escaping as `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`

## Parallelism

WoofWare.NUnitTestRunner has *limited* support for parallelism.
By default, we run tests serially; we may or may not respect the NUnit parallelism attributes to any given extent (but we will never incorrectly run tests in parallel).
For example, as of this writing, we do not run any tests in parallel (but the internal infrastructure is set up so that we will be able to do this soon).

## `TestContext`

WoofWare.NUnitTestRunner has partial support for NUnit's `TestContext`.
See [the test file](./Consumer/TestContext.fs) for everything we expect to work.
