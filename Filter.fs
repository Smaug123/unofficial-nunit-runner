namespace TestRunner

open System
open PrattParser

// Documentation:
// https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests?pivots=mstest

[<RequireQualifiedAccess>]
type FilterIntermediate =
    | FullyQualifiedName
    | Name
    | TestCategory
    | Not of FilterIntermediate
    | Or of FilterIntermediate * FilterIntermediate
    | And of FilterIntermediate * FilterIntermediate
    | Equal of FilterIntermediate * FilterIntermediate
    | Contains of FilterIntermediate * FilterIntermediate
    | String of string

[<RequireQualifiedAccess>]
type TokenType =
    | FullyQualifiedName
    | Name
    | TestCategory
    | OpenParen
    | CloseParen
    | And
    | Or
    | Not
    | Equal
    | NotEqual
    | Contains
    | NotContains
    | String

type Token =
    {
        Type : TokenType
        Trivia : int * int
    }

[<RequireQualifiedAccess>]
module Token =
    let inline standalone (ty : TokenType) (charPos : int) : Token =
        {
            Type = ty
            Trivia = charPos, 1
        }

    let inline single (ty : TokenType) (start : int) (len : int) : Token =
        {
            Type = ty
            Trivia = start, len
        }

    let (|SingleChar|_|) (i : int, c : char) : Token option =
        match c with
        | '(' -> Some (standalone TokenType.OpenParen i)
        | ')' -> Some (standalone TokenType.CloseParen i)
        | '~' -> Some (standalone TokenType.Contains i)
        | '=' -> Some (standalone TokenType.Equal i)
        | '&' -> Some (standalone TokenType.And i)
        | '|' -> Some (standalone TokenType.Or i)
        | '!' -> Some (standalone TokenType.Not i)
        | _ -> None

[<RequireQualifiedAccess>]
module Lexer =
    let lex (s : string) : Token seq =
        seq {
            let mutable i = 0
            let mutable stringAcc : int option = None

            while i < s.Length do
                match (i, s.[i]), stringAcc with
                // This one has to come before the check for prefix Not
                | (startI, '!'), None when i + 1 < s.Length ->
                    i <- i + 1

                    match s.[i] with
                    | '~' ->
                        yield Token.single TokenType.NotContains startI 2
                        i <- i + 1
                    | '=' ->
                        yield Token.single TokenType.NotEqual startI 2
                        i <- i + 1
                    | _ ->
                        yield Token.single TokenType.Not startI 1
                        i <- i + 1
                | Token.SingleChar token, None ->
                    i <- i + 1
                    yield token
                | Token.SingleChar _, Some stringStart ->
                    yield Token.single TokenType.String stringStart (i - stringStart)
                    stringAcc <- None // and we'll do the match again
                | (_, 'F'), None when
                    i + 1 < s.Length
                    && s.[i + 1 ..].StartsWith ("ullyQualifiedName", StringComparison.Ordinal)
                    ->
                    yield Token.single TokenType.FullyQualifiedName i "FullyQualifiedName".Length
                    i <- i + "FullyQualifiedName".Length
                | (_, 'N'), None when i + 1 < s.Length && s.[i + 1 ..].StartsWith ("ame", StringComparison.Ordinal) ->
                    yield Token.single TokenType.Name i "Name".Length
                    i <- i + "Name".Length
                | (_, 'T'), None when
                    i + 1 < s.Length
                    && s.[i + 1 ..].StartsWith ("estCategory", StringComparison.Ordinal)
                    ->
                    yield Token.single TokenType.TestCategory i "TestCategory".Length
                    i <- i + "TestCategory".Length
                | (_, ' '), None -> i <- i + 1
                | (_, _), None ->
                    stringAcc <- Some i
                    i <- i + 1
                | (_, _), Some _ -> i <- i + 1

            match stringAcc with
            | None -> ()
            | Some start -> yield Token.single TokenType.String start (s.Length - start)
        }

[<RequireQualifiedAccess>]
module FilterIntermediate =
    let private atom (inputString : string) (token : Token) : FilterIntermediate option =
        let start, len = token.Trivia

        match token.Type with
        | TokenType.String -> Some (FilterIntermediate.String (inputString.Substring (start, len)))
        | TokenType.FullyQualifiedName -> Some FilterIntermediate.FullyQualifiedName
        | TokenType.Name -> Some FilterIntermediate.Name
        | TokenType.TestCategory -> Some FilterIntermediate.TestCategory
        | TokenType.OpenParen -> None
        | TokenType.CloseParen -> None
        | TokenType.And -> None
        | TokenType.Or -> None
        | TokenType.Not -> None
        | TokenType.NotEqual -> None
        | TokenType.Equal -> None
        | TokenType.NotContains -> None
        | TokenType.Contains -> None

    let parser =
        Parser.make<_, Token, FilterIntermediate> _.Type atom
        |> Parser.withInfix TokenType.And (10, 11) (fun a b -> FilterIntermediate.And (a, b))
        |> Parser.withInfix TokenType.Equal (15, 16) (fun a b -> FilterIntermediate.Equal (a, b))
        |> Parser.withInfix
            TokenType.NotEqual
            (15, 16)
            (fun a b -> FilterIntermediate.Not (FilterIntermediate.Equal (a, b)))
        |> Parser.withInfix TokenType.Contains (15, 16) (fun a b -> FilterIntermediate.Contains (a, b))
        |> Parser.withInfix
            TokenType.NotContains
            (15, 16)
            (fun a b -> FilterIntermediate.Not (FilterIntermediate.Contains (a, b)))
        |> Parser.withInfix TokenType.Or (5, 6) (fun a b -> FilterIntermediate.Or (a, b))
        |> Parser.withUnaryPrefix TokenType.Not ((), 13) FilterIntermediate.Not
        |> Parser.withBracketLike
            TokenType.OpenParen
            {
                ConsumeBeforeInitialToken = false
                ConsumeAfterFinalToken = false
                BoundaryTokens = [ TokenType.CloseParen ]
                Construct = Seq.exactlyOne
            }

    let parse (s : string) : FilterIntermediate =
        let parsed, remaining = Parser.execute parser s (Lexer.lex s |> Seq.toList)

        if not remaining.IsEmpty then
            failwith $"Leftover tokens: %O{remaining}"

        match parsed with
        | FilterIntermediate.String _ -> FilterIntermediate.Contains (FilterIntermediate.FullyQualifiedName, parsed)
        | _ -> parsed

type Match =
    | Exact of string
    | Contains of string

[<RequireQualifiedAccess>]
type Filter =
    | FullyQualifiedName of Match
    | Name of Match
    | TestCategory of Match
    | Not of Filter
    | Or of Filter * Filter
    | And of Filter * Filter

[<RequireQualifiedAccess>]
module Filter =
    let private unescape (s : string) : string =
        // TODO: XML escaping
        s

    let rec make (fi : FilterIntermediate) : Filter =
        match fi with
        | FilterIntermediate.Not x -> Filter.Not (make x)
        | FilterIntermediate.FullyQualifiedName -> failwith "malformed filter: found FullyQualifiedName with no operand"
        | FilterIntermediate.Name -> failwith "malformed filter: found Name with no operand"
        | FilterIntermediate.TestCategory -> failwith "malformed filter: found TestCategory with no operand"
        | FilterIntermediate.Or (a, b) -> Filter.Or (make a, make b)
        | FilterIntermediate.And (a, b) -> Filter.And (make a, make b)
        | FilterIntermediate.Equal (key, value) ->
            let value =
                match value with
                | FilterIntermediate.String s -> unescape s
                | _ -> failwith $"malformed filter: found non-string operand on RHS of equality, '%O{value}'"

            match key with
            | FilterIntermediate.TestCategory -> Filter.TestCategory (Match.Exact value)
            | FilterIntermediate.FullyQualifiedName -> Filter.FullyQualifiedName (Match.Exact value)
            | FilterIntermediate.Name -> Filter.Name (Match.Exact value)
            | _ -> failwith $"Malformed filter: left-hand side of Equals clause must be e.g. TestCategory, was %O{key}"
        | FilterIntermediate.Contains (key, value) ->
            let value =
                match value with
                | FilterIntermediate.String s -> unescape s
                | _ -> failwith $"malformed filter: found non-string operand on RHS of containment, '%O{value}'"

            match key with
            | FilterIntermediate.TestCategory -> Filter.TestCategory (Match.Contains value)
            | FilterIntermediate.FullyQualifiedName -> Filter.FullyQualifiedName (Match.Contains value)
            | FilterIntermediate.Name -> Filter.Name (Match.Contains value)
            | _ ->
                failwith $"Malformed filter: left-hand side of Contains clause must be e.g. TestCategory, was %O{key}"
        | FilterIntermediate.String s ->
            failwith $"Malformed filter: got verbatim string %s{s} when expected an operation"
