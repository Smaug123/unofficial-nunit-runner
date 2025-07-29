namespace WoofWare.NUnitTestRunner

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.Loader
open System.Text
open System.Threading

type internal OutputStreamId = | OutputStreamId of Guid

type private ThreadAwareWriter (local : AsyncLocal<OutputStreamId>, underlying : Dictionary<OutputStreamId, TextWriter>)
    =
    inherit TextWriter ()
    override _.get_Encoding () = Encoding.Default

    override this.Write (v : char) : unit =
        lock
            underlying
            (fun () ->
                match underlying.TryGetValue local.Value with
                | true, output -> output.Write v
                | false, _ ->
                    let wanted =
                        underlying |> Seq.map (fun (KeyValue (a, b)) -> $"%O{a}") |> String.concat "\n"

                    failwith $"no such context: %O{local.Value}\nwanted:\n{wanted}"
            )

    override this.WriteLine (v : string) : unit =
        lock
            underlying
            (fun () ->
                match underlying.TryGetValue local.Value with
                | true, output -> output.WriteLine v
                | false, _ ->
                    let wanted =
                        underlying |> Seq.map (fun (KeyValue (a, b)) -> $"%O{a}") |> String.concat "\n"

                    failwith $"no such context: %O{local.Value}\nwanted:\n{wanted}"
            )

/// Wraps up the necessary context to intercept global state.
[<NoEquality ; NoComparison>]
type TestContexts =
    internal
        {
            /// Accesses to this must be locked on StdOutWriters.
            StdOuts : Dictionary<OutputStreamId, MemoryStream>
            /// Accesses to this must be locked on StdErrWriters.
            StdErrs : Dictionary<OutputStreamId, MemoryStream>
            StdOutWriters : Dictionary<OutputStreamId, TextWriter>
            StdErrWriters : Dictionary<OutputStreamId, TextWriter>
            StdOutWriter : TextWriter
            StdErrWriter : TextWriter
            AsyncLocal : AsyncLocal<OutputStreamId>
        }

    /// Call this exactly once.
    static member Empty () =
        let stdouts = Dictionary ()
        let stderrs = Dictionary ()
        let stdoutWriters = Dictionary ()
        let stderrWriters = Dictionary ()
        let local = AsyncLocal ()
        let stdoutWriter = new ThreadAwareWriter (local, stdoutWriters)
        let stderrWriter = new ThreadAwareWriter (local, stderrWriters)

        {
            StdOuts = stdouts
            StdErrs = stderrs
            StdOutWriter = stdoutWriter
            StdErrWriter = stderrWriter
            StdOutWriters = stdoutWriters
            StdErrWriters = stderrWriters
            AsyncLocal = local
        }

    /// An output stream which will identify the ExecutionContext it's being written to from,
    /// and will separate that output into its own stream internally.
    member this.Stdout : TextWriter = this.StdOutWriter

    /// An output stream which will identify the ExecutionContext it's being written to from,
    /// and will separate that output into its own stream internally.
    member this.Stderr : TextWriter = this.StdErrWriter

    member internal this.DumpStdout (id : OutputStreamId) : string =
        lock
            this.StdOutWriters
            (fun () ->
                this.StdOutWriters.[id].Flush ()
                this.StdOuts.[id].ToArray ()
            )
        |> Encoding.Default.GetString

    member internal this.DumpStderr (id : OutputStreamId) : string =
        lock
            this.StdErrWriters
            (fun () ->
                this.StdErrWriters.[id].Flush ()
                this.StdErrs.[id].ToArray ()
            )
        |> Encoding.Default.GetString

    member internal this.NewOutputs () =
        let id = Guid.NewGuid () |> OutputStreamId
        let msOut = new MemoryStream ()
        let wrOut = new StreamWriter (msOut)
        let msErr = new MemoryStream ()
        let wrErr = new StreamWriter (msErr)

        lock
            this.StdOutWriters
            (fun () ->
                this.StdOutWriters.Add (id, wrOut)
                this.StdOuts.Add (id, msOut)
            )

        lock
            this.StdErrWriters
            (fun () ->
                this.StdErrWriters.Add (id, wrErr)
                this.StdErrs.Add (id, msErr)
            )

        id

    interface IDisposable with
        member this.Dispose () =
            // TODO: dispose the streams
            ()

/// A separate AssemblyLoadContext within which you can run the tests in the given DLL.
/// Supply places to find the .NET runtimes.
type LoadContext (dll : FileInfo, runtimes : DirectoryInfo list, contexts : TestContexts) =
    inherit AssemblyLoadContext ()

    /// Load the assembly with the given name into this assembly context.
    /// This additionally monkey-patches System.Console: it performs SetOut and SetError on them
    /// so that they redirect their outputs into the given `TestContexts`.
    override this.Load (target : AssemblyName) : Assembly =
        let path = Path.Combine (dll.Directory.FullName, $"%s{target.Name}.dll")

        let assy =
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

        if target.Name = "System.Console" then
            if isNull assy then
                failwith "could not monkey-patch System.Console"
            else
                let consoleType = assy.GetType "System.Console"
                let setOut = consoleType.GetMethod "SetOut"
                setOut.Invoke ((null : obj), [| contexts.Stdout |]) |> unbox<unit>
                let setErr = consoleType.GetMethod "SetError"
                setErr.Invoke ((null : obj), [| contexts.Stderr |]) |> unbox<unit>

            assy
        else
            assy
