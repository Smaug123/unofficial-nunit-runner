namespace TestRunner

open Spectre.Console

[<RequireQualifiedAccess>]
module Progress =
    let spectre () : ITestProgress =
        { new ITestProgress with
            member _.OnTestFailed name failure =
                AnsiConsole.Console.MarkupLine
                    $"[red]Test '%s{Markup.Escape name}' failed: %s{Markup.Escape (failure.ToString ())}[/]"

            member _.OnTestFixtureStart name testCount =
                AnsiConsole.Console.MarkupLine $"[white]Running tests: %s{Markup.Escape name}[/]"

            member _.OnTestMemberFinished name =
                AnsiConsole.Console.MarkupLine $"[gray]Finished test: %s{Markup.Escape name}[/]"

            member _.OnTestMemberSkipped name =
                AnsiConsole.Console.MarkupLine $"[yellow]Skipping test due to filter: %s{Markup.Escape name}[/]"

            member _.OnTestMemberStart name =
                AnsiConsole.Console.MarkupLine $"[white]Running test: %s{Markup.Escape name}[/]"
        }
