namespace TestRunner

[<RequireQualifiedAccess>]
module internal Result =

    let inline getError<'r, 'e> (r : Result<'r, 'e>) : 'e option =
        match r with
        | Ok _ -> None
        | Error e -> Some e

    let get<'r, 'e> (r : Result<'r, 'e>) : 'r option =
        match r with
        | Ok r -> Some r
        | Error _ -> None

    let allOkOrError<'o, 'e> (a : Result<'o, 'e> list) : Result<'o list, 'o list * 'e list> =
        let oks = ResizeArray ()
        let errors = ResizeArray ()

        for i in a do
            match i with
            | Error e -> errors.Add e
            | Ok o -> oks.Add o

        let oks = oks |> Seq.toList

        if errors.Count = 0 then
            Ok oks
        else
            Error (oks, Seq.toList errors)
