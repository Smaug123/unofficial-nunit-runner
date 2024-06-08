namespace TestRunner

open System.Xml

[<AutoOpen>]
module internal XmlPatterns =
    [<return : Struct>]
    let (|NodeWithChildren|_|) (expectedName : string) (node : XmlNode) : unit voption =
        if node.Name = expectedName && node.HasChildNodes then
            ValueSome ()
        else
            ValueNone

    let (|NodeWithNamedChild|_|) (childName : string) (node : XmlNode) : XmlNode option =
        if node.HasChildNodes then
            node.ChildNodes
            |> Seq.cast<XmlNode>
            |> Seq.tryFind (fun n -> n.Name = childName)
        else
            None

    let (|OneChildNode|_|) (expectedName : string) (node : XmlNode) : XmlNode option =
        if node.Name = expectedName && node.HasChildNodes && node.ChildNodes.Count = 1 then
            Some node.FirstChild
        else
            None

    let (|NoChildrenNode|_|) (node : XmlNode) : string option =
        if node.HasChildNodes then None else Some node.Value

    [<return : Struct>]
    let (|NamedNoChildren|_|) (name : string) (node : XmlNode) : unit voption =
        if node.HasChildNodes then ValueNone
        elif node.Name = name then ValueSome ()
        else ValueNone

    [<return : Struct>]
    let (|Int64|_|) (s : string) : int64 voption =
        match System.Int64.TryParse s with
        | false, _ -> ValueNone
        | true, v -> ValueSome v

    [<return : Struct>]
    let (|Int|_|) (s : string) : int voption =
        match System.Int32.TryParse s with
        | false, _ -> ValueNone
        | true, v -> ValueSome v
