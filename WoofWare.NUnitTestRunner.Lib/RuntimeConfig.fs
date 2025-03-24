namespace WoofWare.NUnitTestRunner

open System

// Myriad runs the JsonParse generator on this
type internal FrameworkDescription =
    {
        Name : string
        Version : string
    }

// Myriad runs the JsonParse generator on this
type internal RuntimeOptions =
    {
        Tfm : string
        Framework : FrameworkDescription option
        Frameworks : FrameworkDescription list option
        IncludedFramework : FrameworkDescription option
        IncludedFrameworks : FrameworkDescription list option
        RollForward : string option
    }

// Myriad runs the JsonParse generator on this
type internal RuntimeConfig =
    {
        RuntimeOptions : RuntimeOptions
    }

[<RequireQualifiedAccess>]
type internal RollForward =
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
