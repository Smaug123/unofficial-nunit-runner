namespace WoofWare.NUnitTestRunner

open System.Reflection

/// Attributes at the assembly level which control the behaviour of NUnit.
type AssemblyLevelAttributes =
    {
        /// How many tests can be running at once, if anything's running in parallel.
        Parallelism : int option
        /// Whether the tests in this assembly can be parallelised at all.
        Parallelizable : Parallelizable<AssemblyParallelScope> option
    }

[<RequireQualifiedAccess>]
module AssemblyLevelAttributes =

    /// Reflectively obtain the values of any relevant assembly attributes.
    let get (assy : Assembly) : AssemblyLevelAttributes =
        ((None, None), assy.CustomAttributes)
        ||> Seq.fold (fun (levelPar, par) attr ->
            match attr.AttributeType.FullName with
            | "NUnit.Framework.LevelOfParallelismAttribute" ->
                let arg = attr.ConstructorArguments |> Seq.exactlyOne |> _.Value |> unbox<int>

                match levelPar with
                | None -> (Some arg, par)
                | Some existing ->
                    failwith $"Assembly %s{assy.Location} declares parallelism %i{arg} and also %i{existing}"
            | "NUnit.Framework.NonParallelizableAttribute" ->
                match levelPar with
                | None -> (Some 1, par)
                | Some existing ->
                    failwith
                        $"Assembly %s{assy.Location} declares non-parallelizable and also parallelism %i{existing}"
            | "NUnit.Framework.ParallelizableAttribute" ->
                match par with
                | Some _ -> failwith "Got multiple Parallelize attributes in assembly"
                | None ->
                    match attr.ConstructorArguments |> Seq.toList with
                    | [] -> levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Fixtures)
                    | [ v ] ->
                        match v.Value with
                        | :? int as v ->
                            match ParallelScope.ofInt v with
                            | ParallelScope.Fixtures ->
                                levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Fixtures)
                            | ParallelScope.Children ->
                                levelPar, Some (Parallelizable.Yes AssemblyParallelScope.Children)
                            | ParallelScope.None -> levelPar, Some Parallelizable.No
                            | ParallelScope.All ->
                                failwith "ParallelScope.All is invalid on assemblies; only Fixtures or Children"
                            | ParallelScope.Self ->
                                failwith "ParallelScope.Self is invalid on assemblies; only Fixtures or Children"
                        | v -> failwith $"Unexpectedly non-int value %O{v} of parallel scope on assembly"
                    | _ -> failwith "unexpectedly got multiple args to Parallelizable on assembly"
            | _ -> levelPar, par
        )
        |> fun (par, canPar) ->
            {
                Parallelizable = canPar
                Parallelism = par
            }
