using System.Reflection;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace WoofWare.NUnitTestRunner.StartupHookLogic;

public class StartupHookLogic
{
    private static void DoIt()
    {
        var assy = Assembly.GetEntryAssembly()!;
        var assyLoc = new FileInfo(assy.Location);

        // Load FSharp.Core from target
        Assembly.LoadFrom(Path.Combine(assyLoc.Directory!.FullName, "FSharp.Core.dll"));

        // Load test runner lib
        Assembly.LoadFrom(
            "/Users/patrick/Documents/GitHub/WoofWare.NUnitTestRunner/WoofWare.NUnitTestRunner.Lib/bin/Debug/net6.0/WoofWare.NunitTestRunner.Lib.dll");

        var startTime = DateTimeOffset.Now;
        var attrs = AssemblyLevelAttributesModule.get(assy);
        var levelOfParallelism = attrs.Parallelism; // TODO pass arg in
        var testFixtures = assy.ExportedTypes.Select(TestFixtureModule.parse);
        using var par =
            new ParallelQueue(levelOfParallelism, attrs.Parallelizable, FSharpOption<CancellationToken>.None);
        var creationTime = DateTimeOffset.Now;

        static bool NoFilter(TestFixture f, SingleTestMethod g)
        {
            return true;
        }

        var filter = FuncConvert.FromFunc<TestFixture, SingleTestMethod, bool>(NoFilter); // TODO: get the filter
        using var contexts = TestContexts.Empty();
        var results =
            Task.WhenAll(testFixtures.Select(x =>
                TestFixtureModule.run(contexts, par, TestProgress.toStderr(), filter, x)));
        // TODO: timeout bit
        results.Wait();
        var sorted =
            results.Result.SelectMany(x => x);
        var report = BuildTrxReport.build(assy, creationTime, startTime, ListModule.OfSeq(sorted));
        // TODO: trx
        if (report.ResultsSummary.Outcome.Equals(TrxOutcome.Completed))
            Environment.Exit(0);
        else
            Environment.Exit(1);
    }
}
