namespace WoofWare.NUnitTestRunner

[<RequireQualifiedAccess>]
module internal Array =

    let allOkOrError<'o, 'e> (a : Result<'o, 'e>[]) : Result<'o[], 'o[] * 'e[]> =
        let oks = ResizeArray ()
        let errors = ResizeArray ()

        for i in a do
            match i with
            | Error e -> errors.Add e
            | Ok o -> oks.Add o

        let oks = oks.ToArray ()

        if errors.Count = 0 then
            Ok oks
        else
            Error (oks, errors.ToArray ())
