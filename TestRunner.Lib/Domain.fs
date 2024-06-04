namespace TestRunner

open System.Reflection

type Modifier =
    | Explicit of reason : string option
    | Ignored of reason : string option

type TestKind =
    | Single
    | Source of string
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

type TestFailure =
    | TestReturnedNonUnit of obj
    | TestThrew of exn

    override this.ToString () =
        match this with
        | TestFailure.TestReturnedNonUnit ret -> $"Test returned a non-unit: %O{ret}"
        | TestFailure.TestThrew exc -> $"Test threw: %s{exc.Message}\n  %s{exc.StackTrace}"
