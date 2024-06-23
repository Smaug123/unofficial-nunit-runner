using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using WoofWare.NUnitTestRunner.StartupHook;

namespace WoofWare.NUnitTestRunner.StartupHook
{
    internal class StartupAssemblyLoadContext : AssemblyLoadContext
    {
        private readonly AssemblyDependencyResolver _resolver;

        public StartupAssemblyLoadContext()
        {
            _resolver = new AssemblyDependencyResolver(Assembly.GetExecutingAssembly().Location);
        }

        protected override Assembly Load(AssemblyName assemblyName)
        {
            var assemblyPath = _resolver.ResolveAssemblyToPath(assemblyName);

            return assemblyPath != null ? LoadFromAssemblyPath(assemblyPath) : null;
        }
    }
}

// Must be internal and called `StartupHook`
internal class StartupHook
{
    public static void Initialize()
    {
        var loadContext = new StartupAssemblyLoadContext();
        var assembly = loadContext.LoadFromAssemblyName(new AssemblyName("WoofWare.NUnitTestRunner.StartupHookLogic"));
        assembly.DefinedTypes.First(x => x.Name == "StartupHookLogic").GetDeclaredMethod("DoIt")!.Invoke(null, null);
    }
}
