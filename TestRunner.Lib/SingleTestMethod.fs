namespace TestRunner

open System
open System.Reflection

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SingleTestMethod =
    let parse
        (parentCategories : string list)
        (method : MethodInfo)
        (attrs : CustomAttributeData list)
        : SingleTestMethod option * CustomAttributeData list
        =
        let remaining, isTest, hasSource, hasData, modifiers, categories, repeat, comb =
            (([], false, None, None, [], [], None, None), attrs)
            ||> List.fold (fun (remaining, isTest, hasSource, hasData, mods, cats, repeat, comb) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.TestAttribute" ->
                    if attr.ConstructorArguments.Count > 0 then
                        failwith "Unexpectedly got arguments to the Test attribute"

                    (remaining, true, hasSource, hasData, mods, cats, repeat, comb)
                | "NUnit.Framework.TestCaseAttribute" ->
                    let args = attr.ConstructorArguments |> Seq.map _.Value |> Seq.toList

                    match hasData with
                    | None -> (remaining, isTest, hasSource, Some [ List.ofSeq args ], mods, cats, repeat, comb)
                    | Some existing ->
                        (remaining, isTest, hasSource, Some ((List.ofSeq args) :: existing), mods, cats, repeat, comb)
                | "NUnit.Framework.TestCaseSourceAttribute" ->
                    let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    match hasSource with
                    | None -> (remaining, isTest, Some arg, hasData, mods, cats, repeat, comb)
                    | Some existing ->
                        failwith
                            $"Unexpectedly got multiple different sources for test %s{method.Name} (%s{existing}, %s{arg})"
                | "NUnit.Framework.ExplicitAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (remaining, isTest, hasSource, hasData, (Modifier.Explicit reason) :: mods, cats, repeat, comb)
                | "NUnit.Framework.IgnoreAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (remaining, isTest, hasSource, hasData, (Modifier.Ignored reason) :: mods, cats, repeat, comb)
                | "NUnit.Framework.CategoryAttribute" ->
                    let category =
                        attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    (remaining, isTest, hasSource, hasData, mods, category :: cats, repeat, comb)
                | "NUnit.Framework.RepeatAttribute" ->
                    match repeat with
                    | Some _ -> failwith $"Got RepeatAttribute multiple times on %s{method.Name}"
                    | None ->

                    let repeat = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<int>
                    (remaining, isTest, hasSource, hasData, mods, cats, Some repeat, comb)
                | "NUnit.Framework.CombinatorialAttribute" ->
                    match comb with
                    | Some _ ->
                        failwith $"Got CombinatorialAttribute or SequentialAttribute multiple times on %s{method.Name}"
                    | None ->
                        (remaining, isTest, hasSource, hasData, mods, cats, repeat, Some Combinatorial.Combinatorial)
                | "NUnit.Framework.SequentialAttribute" ->
                    match comb with
                    | Some _ ->
                        failwith $"Got CombinatorialAttribute or SequentialAttribute multiple times on %s{method.Name}"
                    | None ->
                        (remaining, isTest, hasSource, hasData, mods, cats, repeat, Some Combinatorial.Sequential)
                | s when s.StartsWith ("NUnit.Framework", StringComparison.Ordinal) ->
                    failwith $"Unrecognised attribute on function %s{method.Name}: %s{attr.AttributeType.FullName}"
                | _ -> (attr :: remaining, isTest, hasSource, hasData, mods, cats, repeat, comb)
            )

        let test =
            match isTest, hasSource, hasData, modifiers, categories, repeat, comb with
            | _, Some _, Some _, _, _, _, _ ->
                failwith
                    $"Test %s{method.Name} unexpectedly has both TestData and TestCaseSource; not currently supported"
            | false, None, None, [], _, _, _ -> None
            | _, Some source, None, mods, categories, repeat, comb ->
                {
                    Kind = TestKind.Source source
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                }
                |> Some
            | _, None, Some data, mods, categories, repeat, comb ->
                {
                    Kind = TestKind.Data data
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                }
                |> Some
            | true, None, None, mods, categories, repeat, comb ->
                {
                    Kind = TestKind.Single
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                }
                |> Some
            | false, None, None, _ :: _, _, _, _ ->
                failwith
                    $"Unexpectedly got test modifiers but no test settings on '%s{method.Name}', which you probably didn't intend."

        test, remaining