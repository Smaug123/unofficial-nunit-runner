namespace TestRunner

#nowarn "9"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Runtime.Loader
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.NativeInterop

[<RequireQualifiedAccess>]
module Native =
    let getMemory' (count : int) (ptr : nativeint) : byte[] =
        let munged = NativePtr.ofNativeInt ptr
        Array.init count (fun i ->
            NativePtr.get<byte> munged i
        )
    let inline getMemory (count : int) (ptr : nativeptr<'a>) : byte[] =
        getMemory' count (NativePtr.toNativeInt ptr)

// Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
// Set AppContext.BaseDirectory to where the test DLL is.
// (This tells the DLL loader to look next to the test DLL for dependencies.)
type SetBaseDir (testDll : FileInfo) =
    let oldBaseDir = AppContext.BaseDirectory
    do AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)

    interface IDisposable with
        member _.Dispose () =
            AppContext.SetData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)


type Ctx (dll : FileInfo, runtimes : DirectoryInfo list) =
    inherit AssemblyLoadContext ()

    override this.Load (target : AssemblyName) : Assembly =
        let path = Path.Combine (dll.Directory.FullName, $"%s{target.Name}.dll")

        if File.Exists path then
            this.LoadFromAssemblyPath path
        else

        runtimes
        |> List.tryPick (fun di ->
            let path = Path.Combine (di.FullName, $"%s{target.Name}.dll")

            if File.Exists path then
                this.LoadFromAssemblyPath path |> Some
            else
                None
        )
        |> Option.defaultValue null


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type DotnetEnvironmentSdkInfoNative =
    {
       size : nativeint
       version : nativeptr<byte>
       path : nativeptr<byte>
    }

[<Struct>]
[<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)>]
type DotnetEnvironmentFrameworkInfoNative =
    {
       size : nativeint
       name : nativeptr<byte>
       version : nativeptr<byte>
       path : nativeptr<byte>
   }

[<Struct>]
[<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)>]
type DotnetEnvironmentInfoNative =
    {
        size : nativeint
        hostfxr_version : nativeptr<byte>
        hostfxr_commit_hash : nativeptr<byte>
        sdk_count : nativeint
        sdks : nativeptr<DotnetEnvironmentSdkInfoNative>

        framework_count : nativeint
        frameworks : nativeptr<DotnetEnvironmentFrameworkInfoNative>
    }

type DotnetEnvironmentFrameworkInfo =
    {
        Name : string
        Version : string
        Path : string
    }

    static member FromNative (n : DotnetEnvironmentFrameworkInfoNative) : DotnetEnvironmentFrameworkInfo =
        if n.size < 0 || n.size > Int32.MaxValue then
            failwith "native int too big or negative"
        let size = int n.size
        if size <> Marshal.SizeOf n then
            failwith "invalid size returned"
        let builder = StringBuilder ()
        let version =
            builder.Clear () |> ignore<StringBuilder>
            let mutable isDone = false
            let mutable i = 0
            while not isDone do
                let b = NativePtr.get n.version i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()
        let path =
            builder.Clear () |> ignore<StringBuilder>
            let mutable isDone = false
            let mutable i = 0
            while not isDone do
                let b = NativePtr.get n.path i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()
        let name =
            builder.Clear () |> ignore<StringBuilder>
            let mutable isDone = false
            let mutable i = 0
            while not isDone do
                let b = NativePtr.get n.name i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()

        {
            Version = version
            Path = path
            Name = name
        }

type DotnetEnvironmentSdkInfo =
    {
        Version : string
        Path : string
    }

    static member FromNative (n : DotnetEnvironmentSdkInfoNative) : DotnetEnvironmentSdkInfo =
        if n.size < 0 || n.size > Int32.MaxValue then
            failwith "native int too big or negative"
        let size = int n.size
        if size <> Marshal.SizeOf n then
            failwith "invalid size returned"

        let builder = StringBuilder ()

        let version =
            builder.Clear () |> ignore<StringBuilder>
            let mutable isDone = false
            let mutable i = 0
            while not isDone do
                let b = NativePtr.get n.version i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()

        let path =
            builder.Clear () |> ignore<StringBuilder>
            let mutable isDone = false
            let mutable i = 0
            while not isDone do
                let b = NativePtr.get n.path i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()

        {
            Version = version
            Path = path
        }

type DotnetEnvironmentInfo =
    {
        mutable HostFxrVersion : string
        mutable HostFxrCommitHash : string
        mutable Sdks : DotnetEnvironmentSdkInfo IReadOnlyList
        mutable Frameworks : DotnetEnvironmentFrameworkInfo IReadOnlyList
    }

    static member FromNative (n : DotnetEnvironmentInfoNative) : DotnetEnvironmentInfo =
        let size = int n.size
        if size <> Marshal.SizeOf n then
            failwith "invalid size returned"
        let builder = StringBuilder ()
        let hostFxrVersion =
            builder.Clear () |> ignore<StringBuilder>
            let mutable i = 0
            let mutable isDone = false
            while not isDone do
                let b = NativePtr.get n.hostfxr_version i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()
        let hostCommitHash =
            builder.Clear () |> ignore<StringBuilder>
            let mutable i = 0
            let mutable isDone = false
            while not isDone do
                let b = NativePtr.get n.hostfxr_commit_hash i
                if b = 0uy then
                    isDone <- true
                else
                    builder.Append (char b) |> ignore<StringBuilder>
                    i <- i + 1
            builder.ToString ()

        let frameworkCount = int n.framework_count
        let frameworks = ResizeArray frameworkCount

        for i = 0 to frameworkCount - 1 do
            frameworks.Add (NativePtr.get n.frameworks i |> DotnetEnvironmentFrameworkInfo.FromNative)

        let sdkCount = int n.sdk_count
        let sdks = ResizeArray sdkCount
        for i = 0 to sdkCount - 1 do
            sdks.Add (NativePtr.get n.sdks i |> DotnetEnvironmentSdkInfo.FromNative)

        {
            Frameworks = frameworks
            HostFxrVersion = hostFxrVersion
            HostFxrCommitHash = hostCommitHash
            Sdks = sdks
        }

type DelegateReturn = delegate of nativeptr<DotnetEnvironmentInfoNative> * nativeint -> unit

type RuntimeDelegate = delegate of nativeptr<byte> * nativeint * nativeint * nativeint -> int32

module Program =
    let storeResult (envInfo: nativeptr<DotnetEnvironmentInfoNative>) (retLoc: nativeint) : unit =
        let toRet = DotnetEnvironmentInfo.FromNative (NativePtr.read envInfo)
        let handle = GCHandle.FromIntPtr retLoc
        handle.set_Target toRet

    let callDelegate (dotnet : string) (f : RuntimeDelegate) : DotnetEnvironmentInfo =
        let dotnet = Encoding.ASCII.GetBytes dotnet
        use fixedDotnet = fixed dotnet

        let mutable extracted = Unchecked.defaultof<DotnetEnvironmentInfo>
        let handle = GCHandle.Alloc extracted

        try
            let callback = Marshal.GetFunctionPointerForDelegate<DelegateReturn> (DelegateReturn storeResult)

            let rc = f.Invoke (fixedDotnet, Unchecked.defaultof<_>, callback, GCHandle.ToIntPtr handle)
            if rc <> 0 then
                failwith $"nonzero exit code %i{rc}"

            handle.Target |> unbox<_>
        finally
            handle.Free ()

    [<TailCall>]
    let rec resolveAllSymlinks (f : FileInfo) : FileInfo =
        match f.LinkTarget with
        | null -> f
        | v -> resolveAllSymlinks (Path.Combine (f.Directory.FullName, v) |> FileInfo)

    /// This is *the most cursed* method. There must surely be a better way.
    let locateRuntimes (dll : FileInfo) : DirectoryInfo list =
        // TODO: test this if we're self-contained
        let hostFxr =
            RuntimeEnvironment.GetRuntimeDirectory ()
            |> DirectoryInfo
            |> fun d -> d.Parent.Parent.Parent
            |> fun d -> Path.Combine (d.FullName, "host", "fxr") |> DirectoryInfo
            |> fun d -> d.EnumerateDirectories () |> Seq.head
            |> fun d -> NativeLibrary.Load (d.EnumerateFiles "*hostfxr*" |> Seq.exactlyOne |> fun f -> f.FullName)
        try
            let ptr = NativeLibrary.GetExport (hostFxr, "hostfxr_get_dotnet_environment_info")
            if ptr = IntPtr.Zero then
                failwith "could not load function"
            let f = Marshal.GetDelegateForFunctionPointer<RuntimeDelegate> ptr
            let result = callDelegate (resolveAllSymlinks (FileInfo "/etc/profiles/per-user/patrick/bin/dotnet")).Directory.FullName f

            printfn $"here: %+A{result}"
        finally
            NativeLibrary.Free hostFxr
        let resolver =
            PathAssemblyResolver
                [|
                    yield dll.FullName
                    yield! Directory.GetFiles (RuntimeEnvironment.GetRuntimeDirectory (), "*.dll")
                    yield! Directory.GetFiles (dll.Directory.FullName, "*.dll")
                |]

        use mlc = new MetadataLoadContext (resolver)
        let assy = mlc.LoadFromAssemblyPath dll.FullName

        let runtime =
            assy.CustomAttributes
            |> Seq.find (fun att -> att.AttributeType.FullName = "System.Runtime.Versioning.TargetFrameworkAttribute")
            |> fun attr -> Seq.exactlyOne attr.ConstructorArguments
            |> fun args -> args.Value |> unbox<string>

        let regex = Regex "\\.NETCoreApp,Version=v([0-9]+)\\.[0-9]+"
        let mat = regex.Match (runtime)

        if not mat.Success then
            failwith $"Could not identify runtime: %s{runtime}"

        let runtimeVersion = mat.Groups.[1].Value |> Int32.Parse

        let runtimes =
            let psi = ProcessStartInfo "dotnet"
            psi.ArgumentList.Add "--list-runtimes"
            psi.RedirectStandardOutput <- true
            use proc = new Process ()
            proc.StartInfo <- psi

            if not (proc.Start ()) then
                failwith "Could not start dotnet"

            let version = proc.StandardOutput.ReadToEnd ()
            proc.WaitForExit ()

            if proc.ExitCode <> 0 then
                failwith $"dotnet quit with exit code %i{proc.ExitCode}"

            version

        runtimes.Split ('\n')
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> Seq.choose (fun runtime ->
            let split = runtime.Split ' '
            let target = split.[1]

            if Int32.Parse (target.Split '.').[0] = runtimeVersion then
                let dir = split.[split.Length - 1]

                Path.Combine (dir.Substring (1, dir.Length - 2), target)
                |> DirectoryInfo
                |> Some
            else
                None
        )
        |> Seq.toList

    let main argv =
        let testDll, filter =
            match argv |> List.ofSeq with
            | [ dll ] -> FileInfo dll, None
            | [ dll ; "--filter" ; filter ] -> FileInfo dll, Some (Filter.parse filter)
            | _ -> failwith "provide exactly one arg, a test DLL"

        let filter =
            match filter with
            | Some filter -> Filter.shouldRun filter
            | None -> fun _ _ -> true

        let progress = Progress.spectre ()

        use _ = new SetBaseDir (testDll)

        let ctx = Ctx (testDll, locateRuntimes testDll)
        let assy = ctx.LoadFromAssemblyPath testDll.FullName

        let anyFailures =
            assy.ExportedTypes
            // TODO: NUnit nowadays doesn't care if you're a TestFixture or not
            |> Seq.filter (fun ty ->
                ty.CustomAttributes
                |> Seq.exists (fun attr -> attr.AttributeType.FullName = "NUnit.Framework.TestFixtureAttribute")
            )
            |> Seq.fold
                (fun anyFailures ty ->
                    let testFixture = TestFixture.parse ty

                    let results = TestFixture.run progress filter testFixture

                    let anyFailures =
                        match results.Failed with
                        | [] -> anyFailures
                        | _ :: _ ->
                            eprintfn $"%i{results.Failed.Length} tests failed"
                            true

                    let anyFailures =
                        match results.OtherFailures with
                        | [] -> anyFailures
                        | otherFailures ->
                            eprintfn "Other failures encountered: "

                            for failure in otherFailures do
                                eprintfn $"  %s{failure.Name}"

                            true

                    anyFailures
                )
                false

        if anyFailures then 1 else 0

    [<EntryPoint>]
    let reallyMain argv =
        // Hack to make sure `finally`s get run.
        // (The runtime doesn't define which `finally`s, if any, run when an uncaught exception terminates execution.)
        try
            main argv
        with _ ->
            reraise ()
