namespace WoofWare.NUnitTestRunner

[<RequireQualifiedAccess>]
module internal Seq =

    let tryMinBy (f : 'a -> 'b) (s : 'a seq) : 'a option =
        use enum = s.GetEnumerator ()

        if not (enum.MoveNext ()) then
            None
        else

        let mutable answer = enum.Current
        let mutable fAnswer = f enum.Current

        while enum.MoveNext () do
            let fNext = f enum.Current

            if fNext < fAnswer then
                answer <- enum.Current

        Some answer
