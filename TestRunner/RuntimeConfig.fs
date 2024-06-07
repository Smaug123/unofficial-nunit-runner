namespace TestRunner

open System
open WoofWare.Myriad.Plugins

[<JsonParse>]
type FrameworkDescription =
    {
        Name : string
        Version : string
    }

[<JsonParse>]
type RuntimeOptions =
    {
        Tfm : string
        Framework : FrameworkDescription
        RollForward : string option
    }

[<JsonParse>]
type RuntimeConfig =
    {
        RuntimeOptions : RuntimeOptions
    }

[<RequireQualifiedAccess>]
type RollForward =
    | Minor
    | Major
    | LatestPatch
    | LatestMinor
    | LatestMajor
    | Disable

    static member Parse (s : string) : RollForward =
        if s.Equals ("minor", StringComparison.OrdinalIgnoreCase) then
            RollForward.Minor
        elif s.Equals ("major", StringComparison.OrdinalIgnoreCase) then
            RollForward.Major
        elif s.Equals ("latestpatch", StringComparison.OrdinalIgnoreCase) then
            RollForward.LatestPatch
        elif s.Equals ("latestminor", StringComparison.OrdinalIgnoreCase) then
            RollForward.LatestMinor
        elif s.Equals ("latestmajor", StringComparison.OrdinalIgnoreCase) then
            RollForward.LatestMajor
        elif s.Equals ("disable", StringComparison.OrdinalIgnoreCase) then
            RollForward.Disable
        else
            failwith $"Could not interpret '%s{s}' as a RollForward"