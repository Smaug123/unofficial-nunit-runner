namespace TestRunner.Test

open System
open NuGet.Packaging.Signing
open TestRunner
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestTrx =

    [<Test>]
    let ``Can parse the first example`` () =
        let resource = EmbeddedResource.read "Example1.trx"

        let parsed =
            match TrxReport.parse resource with
            | Error e -> failwith $"Expected successful parse: %s{e}"
            | Ok r -> r

        parsed.Id.ToString () |> shouldEqual "ccbc0600-fe62-46d9-a3d9-6551da338a3e"
        parsed.Name |> shouldEqual "@Patricks-MacBook-Pro 2024-06-06 22:51:47"

        parsed.Times
        |> shouldEqual
            {
                Creation = DateTimeOffset.Parse "2024-06-06T22:51:47.0155230+01:00"
                Queuing = DateTimeOffset.Parse "2024-06-06T22:51:47.0155230+01:00"
                Start = DateTimeOffset.Parse "2024-06-06T22:51:46.6031940+01:00"
                Finish = DateTimeOffset.Parse "2024-06-06T22:51:47.1574390+01:00"
            }

        parsed.Settings.Name |> shouldEqual "default"

        parsed.Settings.Id.ToString ()
        |> shouldEqual "542f16c8-664f-40a7-9829-0d13fc32f19f"

        parsed.Settings.Deployment
        |> shouldEqual
            {
                RunDeploymentRoot = "_Patricks-MacBook-Pro_2024-06-06_22_51_47"
            }

        do
            parsed.TestDefinitions |> shouldHaveLength 28
            let defn = parsed.TestDefinitions.[0]
            defn.Name |> shouldEqual "Ensure API surface has not been modified"

            defn.Storage
            |> shouldEqual
                "/users/patrick/documents/github/testrunner/testrunner/testrunner.test/bin/debug/net8.0/testrunner.test.dll"

            defn.Id.ToString () |> shouldEqual "1bd5500a-baad-f37f-76eb-3905c00e9884"

            defn.Execution.Id.ToString ()
            |> shouldEqual "663a073f-f01c-400e-bffe-dd8d68355b83"

            defn.TestMethod
            |> shouldEqual
                {
                    CodeBase =
                        "/Users/patrick/Documents/GitHub/TestRunner/TestRunner/TestRunner.Test/bin/Debug/net8.0/TestRunner.Test.dll"
                    AdapterTypeName = Uri "executor://nunit3testexecutor/"
                    ClassName = "TestRunner.Test.TestSurface"
                    Name = "Ensure API surface has not been modified"
                }

        // TODO: test contents of UnitTestResults
        parsed.Results |> shouldHaveLength 28

        do
            parsed.Results.[5].ExecutionId.ToString ()
            |> shouldEqual "663a073f-f01c-400e-bffe-dd8d68355b83"

            parsed.Results.[5].TestId.ToString ()
            |> shouldEqual "1bd5500a-baad-f37f-76eb-3905c00e9884"

            parsed.Results.[5].TestName
            |> shouldEqual "Ensure API surface has not been modified"

            parsed.Results.[5].ComputerName |> shouldEqual "Patricks-MacBook-Pro"
            parsed.Results.[5].Duration |> shouldEqual (TimeSpan.Parse "00:00:00.0329610")

            parsed.Results.[5].StartTime
            |> shouldEqual (DateTimeOffset.Parse "2024-06-06T22:51:47.0636370+01:00")

            parsed.Results.[5].EndTime
            |> shouldEqual (DateTimeOffset.Parse "2024-06-06T22:51:47.0965980+01:00")

            parsed.Results.[5].TestType.ToString ()
            |> shouldEqual "13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b"

            parsed.Results.[5].Outcome |> shouldEqual TrxTestOutcome.Failed

            parsed.Results.[5].TestListId.ToString ()
            |> shouldEqual "8c84fa94-04c1-424b-9868-57a2d4851a1d"

            parsed.Results.[5].RelativeResultsDirectory
            |> shouldEqual "663a073f-f01c-400e-bffe-dd8d68355b83"

            match parsed.Results.[5].Output with
            | None -> failwith "expected output"
            | Some output ->
                match output.ErrorInfo with
                | None -> failwith "expected errorinfo"
                | Some ei ->
                    let message = ei.Message |> Option.get

                    message.StartsWith ("System.Exception : Unexpected", StringComparison.Ordinal)
                    |> shouldEqual true

                    message |> shouldContainText "unit -> string"
                    let stackTrace = ei.StackTrace |> Option.get
                    stackTrace |> shouldContainText "Ensure API surface has not been modified()"

        do
            parsed.TestEntries |> shouldHaveLength 28
            let testEntry = parsed.TestEntries.[0]

            testEntry.TestId.ToString ()
            |> shouldEqual "099ab0d7-e616-e3ed-18e1-8238c2e316c4"

            testEntry.ExecutionId.ToString ()
            |> shouldEqual "0f224761-6bbd-43f2-9559-1327816450d9"

            testEntry.TestListId.ToString ()
            |> shouldEqual "8c84fa94-04c1-424b-9868-57a2d4851a1d"

        do
            parsed.TestLists |> shouldHaveLength 2
            parsed.TestLists.[0].Name |> shouldEqual "Results Not in a List"

            parsed.TestLists.[0].Id.ToString ()
            |> shouldEqual "8c84fa94-04c1-424b-9868-57a2d4851a1d"

            parsed.TestLists.[1].Name |> shouldEqual "All Loaded Results"

            parsed.TestLists.[1].Id.ToString ()
            |> shouldEqual "19431567-8539-422a-85d7-44ee4e166bda"

        let expectedResultsSummary =
            {
                Counters =
                    { TrxCounters.Zero with
                        Total = 28u
                        Executed = 26u
                        Passed = 24u
                        Failed = 2u
                    }
                Output =
                    {
                        StdOut =
                            Some
                                """NUnit Adapter 4.5.0.0: Test execution started
Running all tests in /Users/patrick/Documents/GitHub/TestRunner/TestRunner/TestRunner.Test/bin/Debug/net8.0/TestRunner.Test.dll
   NUnit3TestExecutor discovered 26 of 28 NUnit test cases using Current Discovery mode, Non-Explicit run
Ensure version is monotonic: Not yet published
NUnit Adapter 4.5.0.0: Test execution complete
"""
                        ErrorInfo = None
                    }
                Outcome = TrxOutcome.Failed
                RunInfos =
                    [
                        {
                            ComputerName = "Patricks-MacBook-Pro"
                            Outcome = TrxOutcome.Warning
                            Timestamp = DateTimeOffset.Parse "2024-06-06T22:51:47.0471530+01:00"
                            Text = $"Ok, passed 100 tests.%s{Environment.NewLine}"
                        }
                        {
                            ComputerName = "Patricks-MacBook-Pro"
                            Outcome = TrxOutcome.Warning
                            Timestamp = DateTimeOffset.Parse "2024-06-06T22:51:47.0640360+01:00"
                            Text = $"Ok, passed 100 tests.%s{Environment.NewLine}"
                        }
                    ]
            }

        parsed.ResultsSummary |> shouldEqual expectedResultsSummary
