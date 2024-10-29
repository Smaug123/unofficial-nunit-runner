namespace WoofWare.NUnitTestRunner

open System
open System.Reflection

/// A single method or member which holds some tests. (Often such a member will represent only one test, but e.g.
/// if it has [<TestCaseSource>] then it represents multiple tests.)
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SingleTestMethod =
    /// Extract a SingleTestMethod from the given MethodInfo that we think represents a test.
    /// You pass us the attributes you still haven't parsed from this MethodInfo, and we give you back the sub-list
    /// of attributes we were also unable to interpret.
    /// You also give us the list of categories with which the containing TestFixture is tagged.
    let parse
        (parentCategories : string list)
        (method : MethodInfo)
        (attrs : CustomAttributeData list)
        : SingleTestMethod option * CustomAttributeData list
        =
        let remaining, isTest, sources, hasData, modifiers, categories, repeat, comb, par =
            (([], false, [], None, [], [], None, None, None), attrs)
            ||> List.fold (fun (remaining, isTest, sources, hasData, mods, cats, repeat, comb, par) attr ->
                match attr.AttributeType.FullName with
                | "NUnit.Framework.TestAttribute" ->
                    if attr.ConstructorArguments.Count > 0 then
                        failwith "Unexpectedly got arguments to the Test attribute"

                    (remaining, true, sources, hasData, mods, cats, repeat, comb, par)
                | "NUnit.Framework.TestCaseAttribute" ->
                    let args = attr.ConstructorArguments |> Seq.map _.Value |> Seq.toList

                    let args =
                        match args with
                        | [ :? System.Collections.ICollection as x ] ->
                            x
                            |> Seq.cast<CustomAttributeTypedArgument>
                            |> Seq.map (fun v -> v.Value)
                            |> Seq.toList
                        | _ -> args

                    match hasData with
                    | None -> (remaining, isTest, sources, Some [ List.ofSeq args ], mods, cats, repeat, comb, par)
                    | Some existing ->
                        let args = (List.ofSeq args) :: existing |> Some
                        (remaining, isTest, sources, args, mods, cats, repeat, comb, par)
                | "NUnit.Framework.TestCaseSourceAttribute" ->
                    let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    (remaining, isTest, arg :: sources, hasData, mods, cats, repeat, comb, par)
                | "NUnit.Framework.ExplicitAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (remaining, isTest, sources, hasData, (Modifier.Explicit reason) :: mods, cats, repeat, comb, par)
                | "NUnit.Framework.IgnoreAttribute" ->
                    let reason =
                        attr.ConstructorArguments
                        |> Seq.tryHead
                        |> Option.map (_.Value >> unbox<string>)

                    (remaining, isTest, sources, hasData, (Modifier.Ignored reason) :: mods, cats, repeat, comb, par)
                | "NUnit.Framework.CategoryAttribute" ->
                    let category =
                        attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<string>

                    (remaining, isTest, sources, hasData, mods, category :: cats, repeat, comb, par)
                | "NUnit.Framework.RepeatAttribute" ->
                    match repeat with
                    | Some _ -> failwith $"Got RepeatAttribute multiple times on %s{method.Name}"
                    | None ->

                    let repeat = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<int>
                    (remaining, isTest, sources, hasData, mods, cats, Some repeat, comb, par)
                | "NUnit.Framework.CombinatorialAttribute" ->
                    match comb with
                    | Some _ ->
                        failwith $"Got CombinatorialAttribute or SequentialAttribute multiple times on %s{method.Name}"
                    | None ->
                        (remaining, isTest, sources, hasData, mods, cats, repeat, Some Combinatorial.Combinatorial, par)
                | "NUnit.Framework.SequentialAttribute" ->
                    match comb with
                    | Some _ ->
                        failwith $"Got CombinatorialAttribute or SequentialAttribute multiple times on %s{method.Name}"
                    | None ->
                        (remaining, isTest, sources, hasData, mods, cats, repeat, Some Combinatorial.Sequential, par)
                | "NUnit.Framework.NonParallelizableAttribute" ->
                    match par with
                    | Some _ -> failwith $"Got a parallelization attribute multiple times on %s{method.Name}"
                    | None -> (remaining, isTest, sources, hasData, mods, cats, repeat, comb, Some Parallelizable.No)
                | "NUnit.Framework.ParallelizableAttribute" ->
                    match par with
                    | Some _ -> failwith $"Got multiple parallelization attributes on %s{method.Name}"
                    | None ->
                        let arg =
                            match Seq.toList attr.ConstructorArguments with
                            | [] -> Parallelizable.Yes ()
                            | [ x ] ->
                                if x.ArgumentType.Name <> "ParallelScope" then
                                    failwith
                                        $"Got argument %O{x.Value} of unrecognised type %s{x.ArgumentType.Name} on [<Parallelizable>] attribute; expected ParallelScope"

                                match ParallelScope.ofInt (unbox<int> x.Value) with
                                | ParallelScope.Children ->
                                    failwith
                                        $"Unexpected ParallelScope.Children on test %s{method.Name}; this is not valid on individual tests"
                                | ParallelScope.Fixtures ->
                                    failwith
                                        $"Unexpected ParallelScope.Children on test %s{method.Name}; this is not valid on individual tests"
                                | ParallelScope.All
                                | ParallelScope.Self -> Parallelizable.Yes ()
                                | ParallelScope.None -> Parallelizable.No
                            | s -> failwith $"Got multiple arguments on a [<Parallelizable>] attribute: %O{s}"

                        (remaining, isTest, sources, hasData, mods, cats, repeat, comb, Some arg)
                | s when s.StartsWith ("NUnit.Framework", StringComparison.Ordinal) ->
                    failwith $"Unrecognised attribute on function %s{method.Name}: %s{attr.AttributeType.FullName}"
                | _ -> (attr :: remaining, isTest, sources, hasData, mods, cats, repeat, comb, par)
            )

        let test =
            match isTest, sources, hasData, modifiers, categories, repeat, comb, par with
            | _, _ :: _, Some _, _, _, _, _, _ ->
                failwith
                    $"Test '%s{method.Name}' unexpectedly has both TestData and TestCaseSource; not currently supported"
            | false, [], None, [], _, _, _, _ -> None
            | _, _ :: _, None, mods, categories, repeat, comb, par ->
                {
                    Kind = TestKind.Source sources
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                    Parallelize = par
                }
                |> Some
            | _, [], Some data, mods, categories, repeat, comb, par ->
                {
                    Kind = TestKind.Data data
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                    Parallelize = par
                }
                |> Some
            | true, [], None, mods, categories, repeat, comb, par ->
                {
                    Kind = TestKind.Single
                    Method = method
                    Modifiers = mods
                    Categories = categories @ parentCategories
                    Repeat = repeat
                    Combinatorial = comb
                    Parallelize = par
                }
                |> Some
            | false, [], None, _ :: _, _, _, _, _ ->
                failwith
                    $"Unexpectedly got test modifiers but no test settings on '%s{method.Name}', which you probably didn't intend."

        test, remaining
