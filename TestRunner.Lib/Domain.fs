namespace TestRunner

open System.Reflection

[<RequireQualifiedAccess>]
type Modifier =
    | Explicit of reason : string option
    | Ignored of reason : string option

[<RequireQualifiedAccess>]
type TestKind =
    | Single
    | Source of string list
    | Data of obj list list

type Combinatorial =
    | Combinatorial
    | Sequential

type SingleTestMethod =
    {
        Method : MethodInfo
        Kind : TestKind
        Modifiers : Modifier list
        Categories : string list
        Repeat : int option
        Combinatorial : Combinatorial option
    }

    member this.Name = this.Method.Name

type TestFixture =
    {
        Name : string
        OneTimeSetUp : MethodInfo option
        OneTimeTearDown : MethodInfo option
        SetUp : MethodInfo list
        TearDown : MethodInfo list
        Tests : SingleTestMethod list
    }

    static member Empty (name : string) =
        {
            Name = name
            OneTimeSetUp = None
            OneTimeTearDown = None
            SetUp = []
            TearDown = []
            Tests = []
        }

[<RequireQualifiedAccess>]
type UserMethodFailure =
    | ReturnedNonUnit of name : string * result : obj
    | Threw of name : string * exn

    override this.ToString () =
        match this with
        | UserMethodFailure.ReturnedNonUnit (method, ret) ->
            $"User-defined method %s{method} returned a non-unit: %O{ret}"
        | UserMethodFailure.Threw (method, exc) ->
            $"User-defined method %s{method} threw: %s{exc.Message}\n  %s{exc.StackTrace}"

[<RequireQualifiedAccess>]
type TestFailure =
    | TestFailed of UserMethodFailure
    | SetUpFailed of UserMethodFailure
    | TearDownFailed of UserMethodFailure
