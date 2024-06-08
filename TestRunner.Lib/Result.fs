namespace TestRunner

[<RequireQualifiedAccess>]
module internal Result =

    let inline getError<'r, 'e> (r : Result<'r, 'e>) : 'e option =
        match r with
        | Ok _ -> None
        | Error e -> Some e

    let inline get<'r, 'e> (r : Result<'r, 'e>) : 'r option =
        match r with
        | Ok r -> Some r
        | Error _ -> None
