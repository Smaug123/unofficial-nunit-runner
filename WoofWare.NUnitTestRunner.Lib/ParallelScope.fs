namespace WoofWare.NUnitTestRunner

/// Our own strongly-typed rendering of the NUnit ParallelScope enum.
/// This is more tightly modelled by ClassParallelScope and AssemblyParallelScope in our own domain; this type exists
/// for the initial interop.
[<RequireQualifiedAccess>]
type ParallelScope =
    /// Corresponds to NUnit's ParallelScope.Fixtures.
    | Fixtures
    /// Corresponds to NUnit's ParallelScope.Children.
    | Children
    /// Corresponds to NUnit's ParallelScope.All.
    | All
    /// Corresponds to NUnit's ParallelScope.Self.
    | Self
    /// Corresponds to NUnit's ParallelScope.None.
    | None

[<RequireQualifiedAccess>]
module ParallelScope =
    let ofInt (n : int) =
        match n with
        | 512 -> ParallelScope.Fixtures
        | 256 -> ParallelScope.Children
        | 257 -> ParallelScope.All
        | 1 -> ParallelScope.Self
        | 2 -> ParallelScope.None
        | _ -> failwith $"Unrecognised ParallelScope enum: %i{n}"
