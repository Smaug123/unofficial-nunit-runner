using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using WoofWare.NUnitTestRunner;
using AssemblyLevelAttributesModule = WoofWare.NUnitTestRunner.AssemblyLevelAttributesModule;
using TestFixtureModule = WoofWare.NUnitTestRunner.TestFixtureModule;

class StartupAssemblyLoadContext : AssemblyLoadContext
{
    private readonly AssemblyDependencyResolver _resolver;

    public StartupAssemblyLoadContext()
    {
        _resolver = new AssemblyDependencyResolver(Assembly.GetExecutingAssembly().Location);
    }

    protected override Assembly Load(AssemblyName assemblyName)
    {
        string assemblyPath = _resolver.ResolveAssemblyToPath(assemblyName);

        if (assemblyPath != null)
        {
            return LoadFromAssemblyPath(assemblyPath);
        }

        return null;
    }
}

internal class StartupHook
{
    public static void Initialize()
    {
        var loadContext = new StartupAssemblyLoadContext();
        var assembly = loadContext.LoadFromAssemblyName(new AssemblyName("StartupHookLogic"));
        assembly.DefinedTypes.Where(x => x.Name == "Class1").First().GetDeclaredMethod("DoIt").Invoke(null, null);
    }
}
