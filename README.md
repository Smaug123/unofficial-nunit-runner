# Toy NUnit test runner

## Filtering

To supply special characters in a string, XML-encode them; we will XML-decode the string before parsing it.
We support at least the [documented `dotnet test` examples](https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests), with the following exception:

* `MyNamespace.MyTestsClass<ParameterType1%2CParameterType2>.MyTestMethod`. To supply this name, put it in quotes and XML-escape it: `"MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`
