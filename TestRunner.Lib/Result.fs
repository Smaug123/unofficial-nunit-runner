namespace TestRunner

[<RequireQualifiedAccess>]
module internal Result =

    let getError<'r, 'e> (r : Result<'r, 'e>) : 'e option =
        match r with
        | Ok _ -> None
        | Error e -> Some e
