namespace WoofWare.NUnitTestRunner

open Spectre.Console

[<RequireQualifiedAccess>]
module Progress =
    let spectre (console : IAnsiConsole) : ITestProgress =
        { new ITestProgress with
            member _.OnTestFailed name failure =
                console.MarkupLine
                    $"[red]Test '%s{Markup.Escape name}' failed: %s{Markup.Escape (failure.ToString ())}[/]"

            member _.OnTestFixtureStart name testCount =
                console.MarkupLine $"[white]Running tests: %s{Markup.Escape name}[/]"

            member _.OnTestMemberFinished name =
                console.MarkupLine $"[gray]Finished test: %s{Markup.Escape name}[/]"

            member _.OnTestMemberSkipped name =
                console.MarkupLine $"[yellow]Skipping test due to filter: %s{Markup.Escape name}[/]"

            member _.OnTestMemberStart name =
                console.MarkupLine $"[white]Running test: %s{Markup.Escape name}[/]"
        }
