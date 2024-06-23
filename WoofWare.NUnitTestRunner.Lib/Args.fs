namespace WoofWare.NUnitTestRunner

open System
open System.IO

[<AutoOpen>]
module internal Patterns =
    let (|Key|_|) (start : string) (s : string) : string option =
        if s.StartsWith (start + "=", StringComparison.Ordinal) then
            s.Substring (start.Length + 1) |> Some
        else
            None

/// Represents how verbose the test runner's logging should be.
[<RequireQualifiedAccess>]
type LogLevel =
    /// Don't log any information about the test run.
    | Nothing
    /// Log as much information as is available about the test run.
    | Verbose

/// Arguments controlling the test runner itself (not the tests therein).
type Args =
    {
        /// The DLL containing the tests we'll reflectively discover and invoke.
        Dll : FileInfo
        /// If set, the output file into which we will write a TRX report. (We'll create parent directories as necessary.)
        Trx : FileInfo option
        /// Also contains the original string which specified the filter.
        Filter : (string * Filter) option
        /// How verbose to be with the test runner's own logging.
        Logging : LogLevel
        /// Maximum number of tests which can run concurrently. This setting overrides any LevelOfParallelism reflectively
        /// extracted from the assembly under test.
        LevelOfParallelism : int option
        /// Abort if the test runner is running for longer than this timeout.
        Timeout : TimeSpan option
    }

    /// Parse `argv` into a structured Args.
    static member Parse (args : string list) : Args =
        match args with
        | [] -> failwith "The first arg must be a positional arg, the DLL to test."
        | dll :: args ->

        let rec go
            (trx : FileInfo option)
            (filter : (string * Filter) option)
            (logging : LogLevel option)
            (par : int option)
            (timeout : TimeSpan option)
            (args : string list)
            =
            match args with
            | [] ->
                {
                    Dll = FileInfo dll
                    Trx = trx
                    Filter = filter
                    Logging = logging |> Option.defaultValue LogLevel.Nothing
                    LevelOfParallelism = par
                    Timeout = timeout
                }
            | Key "--filter" filterStr :: rest
            | "--filter" :: filterStr :: rest ->
                match filter with
                | Some _ -> failwith "Two conflicting filters; you can only specify --filter once"
                | None -> go trx (Some (filterStr, Filter.parse filterStr)) logging par timeout rest
            | Key "--trx" trxStr :: rest
            | "--trx" :: trxStr :: rest ->
                match trx with
                | Some _ -> failwith "Two conflicting TRX outputs; you can only specify --trx once"
                | None -> go (Some (FileInfo trxStr)) filter logging par timeout rest
            | Key "--verbose" verboseStr :: rest
            | "--verbose" :: verboseStr :: rest ->
                match logging with
                | Some _ -> failwith "Two conflicting --verbose outputs; you can only specify --verbose once"
                | None ->
                    let verbose =
                        if Boolean.Parse verboseStr then
                            LogLevel.Verbose
                        else
                            LogLevel.Nothing

                    go trx filter (Some verbose) par timeout rest
            | Key "--parallelism" parStr :: rest
            | "--parallelism" :: parStr :: rest ->
                match par with
                | Some _ -> failwith "Two conflicting --parallelism outputs; you can only specify --parallelism once"
                | None -> go trx filter logging (Some (Int32.Parse parStr)) timeout rest
            | Key "--timeout-seconds" timeoutStr :: rest
            | "--timeout-seconds" :: timeoutStr :: rest ->
                match timeout with
                | Some _ ->
                    failwith "Two conflicting --timeout-seconds outputs; you can only specify --timeout-seconds once"
                | None -> go trx filter logging par (Some (TimeSpan.FromSeconds (Int32.Parse timeoutStr |> float))) rest
            | k :: _rest -> failwith $"Unrecognised arg %s{k}"

        go None None None None None args
