namespace WoofWare.NUnitTestRunner

open System
open System.IO
open System.Reflection

// Fix for https://github.com/Smaug123/unofficial-nunit-runner/issues/8
// Set AppContext.BaseDirectory to where the test DLL is.
// (This tells the DLL loader to look next to the test DLL for dependencies.)
type SetBaseDir (testDll : FileInfo) =
    let oldBaseDir = AppContext.BaseDirectory

    let setData =
        let appContext = Type.GetType "System.AppContext"

        if Object.ReferenceEquals (appContext, (null : obj)) then
            ignore<string * string>
        else

        let setDataMethod =
            appContext.GetMethod ("SetData", BindingFlags.Static ||| BindingFlags.Public)

        if Object.ReferenceEquals (setDataMethod, (null : obj)) then
            ignore<string * string>
        else

        fun (k, v) -> setDataMethod.Invoke (null, [| k ; v |]) |> unbox<unit>

    do setData ("APP_CONTEXT_BASE_DIRECTORY", testDll.Directory.FullName)

    interface IDisposable with
        member _.Dispose () =
            setData ("APP_CONTEXT_BASE_DIRECTORY", oldBaseDir)
