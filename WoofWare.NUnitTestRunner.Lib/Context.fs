namespace WoofWare.NUnitTestRunner

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.Loader
open System.Text
open System.Threading
open System.Threading.Tasks

type private ThreadAwareWriter
    (local : AsyncLocal<Guid>, underlying : Dictionary<Guid, TextWriter>, mem : Dictionary<Guid, MemoryStream>)
    =
    inherit TextWriter ()
    override _.get_Encoding () = Encoding.Default

    member internal _.DumpOutput () : string Task =
        use prev = ExecutionContext.Capture ()

        let tcs = TaskCompletionSource<_> ()

        (fun _ ->
            (fun () ->
                match mem.TryGetValue local.Value with
                | true, output -> tcs.SetResult (output.ToArray ())
                | false, _ ->
                    let wanted =
                        mem |> Seq.map (fun (KeyValue (a, b)) -> $"%O{a}") |> String.concat "\n"

                    failwith $"no such context: %O{local.Value}\nwanted:\n"
            )
            |> lock underlying
        )
        |> fun action -> ExecutionContext.Run (prev, action, ())

        task {
            let! bytes = tcs.Task
            return Encoding.Default.GetString bytes
        }

    override this.Write (v : char) : unit =
        use prev = ExecutionContext.Capture ()

        (fun _ ->
            (fun () ->
                match underlying.TryGetValue local.Value with
                | true, output -> output.Write v
                | false, _ ->
                    let wanted =
                        underlying |> Seq.map (fun (KeyValue (a, b)) -> $"%O{a}") |> String.concat "\n"

                    failwith $"no such context: %O{local.Value}\nwanted:\n"
            )
            |> lock underlying
        )
        |> fun action -> ExecutionContext.Run (prev, action, ())

    override this.WriteLine (v : string) : unit =
        use prev = ExecutionContext.Capture ()

        (fun _ ->
            (fun () ->
                match underlying.TryGetValue local.Value with
                | true, output -> output.WriteLine v
                | false, _ ->
                    let wanted =
                        underlying |> Seq.map (fun (KeyValue (a, b)) -> $"%O{a}") |> String.concat "\n"

                    failwith $"no such context: %O{local.Value}\nwanted:\n"
            )
            |> lock underlying
        )
        |> fun action -> ExecutionContext.Run (prev, action, ())

/// Wraps up the necessary context to intercept global state.
type TestContexts =
    private
        {
            /// Accesses to this must be locked on StdOutWriters.
            StdOuts : Dictionary<Guid, MemoryStream>
            /// Accesses to this must be locked on StdErrWriters.
            StdErrs : Dictionary<Guid, MemoryStream>
            StdOutWriters : Dictionary<Guid, TextWriter>
            StdErrWriters : Dictionary<Guid, TextWriter>
            StdOutWriter : TextWriter
            StdErrWriter : TextWriter
            AsyncLocal : AsyncLocal<Guid>
        }

    /// Call this exactly once.
    static member Empty () =
        let stdouts = Dictionary ()
        let stderrs = Dictionary ()
        let stdoutWriters = Dictionary ()
        let stderrWriters = Dictionary ()
        let local = AsyncLocal ()
        let stdoutWriter = new ThreadAwareWriter (local, stdoutWriters, stdouts)
        let stderrWriter = new ThreadAwareWriter (local, stderrWriters, stderrs)

        {
            StdOuts = stdouts
            StdErrs = stderrs
            StdOutWriter = stdoutWriter
            StdErrWriter = stderrWriter
            StdOutWriters = stdoutWriters
            StdErrWriters = stderrWriters
            AsyncLocal = local
        }

    member internal this.Stdout : TextWriter = this.StdOutWriter
    member internal this.Stderr : TextWriter = this.StdErrWriter

    member internal this.DumpStdout (id : Guid) : string =
        lock
            this.StdOutWriters
            (fun () ->
                this.StdOutWriters.[id].Flush ()
                this.StdOuts.[id].ToArray ()
            )
        |> Encoding.Default.GetString

    member internal this.DumpStderr (id : Guid) : string =
        lock
            this.StdErrWriters
            (fun () ->
                this.StdErrWriters.[id].Flush ()
                this.StdErrs.[id].ToArray ()
            )
        |> Encoding.Default.GetString

    member internal this.NewOutputs () =
        let id = Guid.NewGuid ()
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
