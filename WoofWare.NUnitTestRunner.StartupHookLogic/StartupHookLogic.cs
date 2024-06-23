using System.Reflection;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace WoofWare.NUnitTestRunner.StartupHookLogic;

public class StartupHookLogic
{
    private static void DoIt()
    {
        // Load test runner lib
        var nunitLib = Environment.GetEnvironmentVariable("WOOFWARE_NUNIT_LIB");
        if (string.IsNullOrEmpty(nunitLib))
        {
            throw new ArgumentException("WoofWare.NUnitTestRunner hook expects to run with WOOFWARE_NUNIT_LIB set to WoofWare.NUnitTestRunner.Lib.dll");
        }
        Assembly.LoadFrom(nunitLib);

        var startTime = DateTimeOffset.Now;

        // Arg parsing

        var filterVar = Environment.GetEnvironmentVariable("WOOFWARE_NUNIT_FILTER");
        static bool NoFilter(TestFixture f, SingleTestMethod g)
        {
            return true;
        }

        FSharpFunc<TestFixture, FSharpFunc<SingleTestMethod, bool>> filter;
        if (string.IsNullOrEmpty(filterVar))
        {
            filter = FuncConvert.FromFunc<TestFixture, SingleTestMethod, bool>(NoFilter);
        }
        else
        {
            filter = FilterModule.shouldRun(FilterModule.parse(filterVar));
        }

        var assy = Assembly.GetEntryAssembly()!;

        var attrs = AssemblyLevelAttributesModule.get(assy);

        FSharpOption<int> levelOfParallelism;
        var parallelismVar = Environment.GetEnvironmentVariable("WOOFWARE_NUNIT_PARALLELISM_LEVEL");
        if (string.IsNullOrEmpty(parallelismVar))
        {
            levelOfParallelism = attrs.Parallelism;
        }
        else
        {
            levelOfParallelism = FSharpOption<int>.Some(Int32.Parse(parallelismVar));
        }

        var testFixtures = assy.ExportedTypes.Select(TestFixtureModule.parse);
        using var par =
            new ParallelQueue(levelOfParallelism, attrs.Parallelizable, FSharpOption<CancellationToken>.None);
        var creationTime = DateTimeOffset.Now;

        var timeoutVar = Environment.GetEnvironmentVariable("WOOFWARE_NUNIT_TIMEOUT_SECS");
        TimeSpan timeout;
        if (string.IsNullOrEmpty(timeoutVar))
        {
            timeout = TimeSpan.FromHours(2.0);
        }
        else
        {
            timeout = TimeSpan.FromSeconds(Int32.Parse(timeoutVar));
        }

        using var contexts = TestContexts.Empty();
        var results =
            Task.WhenAll(testFixtures.Select(x =>
                TestFixtureModule.run(contexts, par, TestProgress.toStderr(), filter, x)));

        if (!results.Wait(timeout))
        {
            throw new Exception($"Tests failed to terminate within timeout of {timeout}");
        }

        var sorted =
            results.Result.SelectMany(x => x);
        var report = BuildTrxReport.build(assy, creationTime, startTime, ListModule.OfSeq(sorted));

        var trxFile = Environment.GetEnvironmentVariable("WOOFWARE_NUNIT_GENERATE_TRX");
        if (!string.IsNullOrEmpty(trxFile))
        {
            var contents = TrxReportModule.toXml(report).OuterXml;
            var trxPath = new FileInfo(trxFile);
            trxPath.Directory!.Create();
            File.WriteAllText(trxPath.FullName, contents);
            Console.Error.WriteLine($"Written TRX file: {trxPath.FullName}");
        }

        if (report.ResultsSummary.Outcome.Equals(TrxOutcome.Completed))
            Environment.Exit(0);
        else
            Environment.Exit(1);
    }
}
