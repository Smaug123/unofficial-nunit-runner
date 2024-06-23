namespace Consumer

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Text
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module RunSubProcess =
    [<Test>]
    let ``Run a subprocess`` () =
        let exe = "/bin/bash"
        let args = [ "-c" ; "echo hi >&2 && echo bye" ]
        let workingDir = None

        let psi =
            ProcessStartInfo (
                exe,
                UseShellExecute = false,
                RedirectStandardError = true,
                RedirectStandardOutput = true,
                WorkingDirectory = Option.toObj workingDir
            )

        for arg in args do
            psi.ArgumentList.Add arg

        psi.EnvironmentVariables.Add ("THING", Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "hi"))
        let stderr = StringBuilder ()
        use proc = new Process (StartInfo = psi)
        proc.OutputDataReceived.Add (fun e -> printfn $"%s{e.Data}")

        proc.ErrorDataReceived.Add (fun e ->
            eprintfn $"%s{e.Data}"
            stderr.AppendLine e.Data |> ignore
        )

        proc.Start () |> shouldEqual true
        proc.BeginOutputReadLine ()
        proc.BeginErrorReadLine ()

        proc.WaitForExit ()
