namespace WoofWare.NUnitTestRunner

[<RequireQualifiedAccess>]
module internal List =

    /// Given e.g. [[1,2],[4,5,6]], returns:
    /// [1;4] ; [1;5] ; [1;6] ; [2;4] ; [2;5] ; [2;6]
    /// in some order.
    /// This is like allPairs but more so.
    let rec combinations (s : 'a list list) : 'a list list =
        match s with
        | [] -> [ [] ]
        | head :: s ->
            let sub = combinations s
            head |> List.collect (fun head -> sub |> List.map (fun tail -> head :: tail))
