namespace TestRunner.Test

open System
open System.IO

[<RequireQualifiedAccess>]
module internal EmbeddedResource =
    type Dummy = class end

    let read (name : string) : string =
        let assy = typeof<Dummy>.Assembly
        let manifestName =
            assy.GetManifestResourceNames ()
            |> Seq.filter (fun s -> s.EndsWith (name, StringComparison.Ordinal))
            |> Seq.exactlyOne
        use s = assy.GetManifestResourceStream manifestName
        use reader = new StreamReader (s)
        reader.ReadToEnd ()
