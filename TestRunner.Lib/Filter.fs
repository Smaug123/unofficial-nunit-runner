namespace TestRunner

open System
open PrattParser

// Documentation:
// https://learn.microsoft.com/en-us/dotnet/core/testing/selective-unit-tests?pivots=mstest

[<RequireQualifiedAccess>]
type internal ParsedFilter =
    | FullyQualifiedName
    | Name
    | TestCategory
    | Not of ParsedFilter
    | Or of ParsedFilter * ParsedFilter
    | And of ParsedFilter * ParsedFilter
    | Equal of ParsedFilter * ParsedFilter
    | Contains of ParsedFilter * ParsedFilter
    | String of string

[<RequireQualifiedAccess>]
type internal TokenType =
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

type internal Token =
    {
        Type : TokenType
        Trivia : int * int
    }

[<RequireQualifiedAccess>]
module internal Token =
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
module internal Lexer =
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
module internal ParsedFilter =
    let private atom (inputString : string) (token : Token) : ParsedFilter option =
        let start, len = token.Trivia

        match token.Type with
        | TokenType.String -> Some (ParsedFilter.String (inputString.Substring (start, len)))
        | TokenType.FullyQualifiedName -> Some ParsedFilter.FullyQualifiedName
        | TokenType.Name -> Some ParsedFilter.Name
        | TokenType.TestCategory -> Some ParsedFilter.TestCategory
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
        Parser.make<_, Token, ParsedFilter> _.Type atom
        |> Parser.withInfix TokenType.And (10, 11) (fun a b -> ParsedFilter.And (a, b))
        |> Parser.withInfix TokenType.Equal (15, 16) (fun a b -> ParsedFilter.Equal (a, b))
        |> Parser.withInfix TokenType.NotEqual (15, 16) (fun a b -> ParsedFilter.Not (ParsedFilter.Equal (a, b)))
        |> Parser.withInfix TokenType.Contains (15, 16) (fun a b -> ParsedFilter.Contains (a, b))
        |> Parser.withInfix TokenType.NotContains (15, 16) (fun a b -> ParsedFilter.Not (ParsedFilter.Contains (a, b)))
        |> Parser.withInfix TokenType.Or (5, 6) (fun a b -> ParsedFilter.Or (a, b))
        |> Parser.withUnaryPrefix TokenType.Not ((), 13) ParsedFilter.Not
        |> Parser.withBracketLike
            TokenType.OpenParen
            {
                ConsumeBeforeInitialToken = false
                ConsumeAfterFinalToken = false
                BoundaryTokens = [ TokenType.CloseParen ]
                Construct = List.exactlyOne
            }

    let parse (s : string) : ParsedFilter =
        let parsed, remaining = Parser.execute parser s (Lexer.lex s |> Seq.toList)

        if not remaining.IsEmpty then
            failwith $"Leftover tokens: %O{remaining}"

        match parsed with
        | ParsedFilter.String _ -> ParsedFilter.Contains (ParsedFilter.FullyQualifiedName, parsed)
        | _ -> parsed

/// The type of matching which this filter will perform.
type Match =
    /// This filter will only match if its argument is exactly (case-sensitively) equal to this.
    | Exact of string
    /// This filter will match if its argument (case-sensitively) contains this substring.
    | Contains of string

/// A filter which is to be applied when running tests, to determine which tests to run.
[<RequireQualifiedAccess>]
type Filter =
    /// The fully qualified name of the test must match this.
    | FullyQualifiedName of Match
    /// The name (without its assembly prepended) of the test must match this.
    | Name of Match
    /// The test must be in a matching category.
    | TestCategory of Match
    /// The test must not match this filter.
    | Not of Filter
    /// The test must match at least one of these filters.
    | Or of Filter * Filter
    /// The test must match both of these filters.
    | And of Filter * Filter

/// Methods for manipulating filters.
[<RequireQualifiedAccess>]
module Filter =
    let private unescape (s : string) : string =
        // TODO: XML escaping
        s

    let rec internal makeParsed (fi : ParsedFilter) : Filter =
        match fi with
        | ParsedFilter.Not x -> Filter.Not (makeParsed x)
        | ParsedFilter.FullyQualifiedName -> failwith "malformed filter: found FullyQualifiedName with no operand"
        | ParsedFilter.Name -> failwith "malformed filter: found Name with no operand"
        | ParsedFilter.TestCategory -> failwith "malformed filter: found TestCategory with no operand"
        | ParsedFilter.Or (a, b) -> Filter.Or (makeParsed a, makeParsed b)
        | ParsedFilter.And (a, b) -> Filter.And (makeParsed a, makeParsed b)
        | ParsedFilter.Equal (key, value) ->
            let value =
                match value with
                | ParsedFilter.String s -> unescape s
                | _ -> failwith $"malformed filter: found non-string operand on RHS of equality, '%O{value}'"

            match key with
            | ParsedFilter.TestCategory -> Filter.TestCategory (Match.Exact value)
            | ParsedFilter.FullyQualifiedName -> Filter.FullyQualifiedName (Match.Exact value)
            | ParsedFilter.Name -> Filter.Name (Match.Exact value)
            | _ -> failwith $"Malformed filter: left-hand side of Equals clause must be e.g. TestCategory, was %O{key}"
        | ParsedFilter.Contains (key, value) ->
            let value =
                match value with
                | ParsedFilter.String s -> unescape s
                | _ -> failwith $"malformed filter: found non-string operand on RHS of containment, '%O{value}'"

            match key with
            | ParsedFilter.TestCategory -> Filter.TestCategory (Match.Contains value)
            | ParsedFilter.FullyQualifiedName -> Filter.FullyQualifiedName (Match.Contains value)
            | ParsedFilter.Name -> Filter.Name (Match.Contains value)
            | _ ->
                failwith $"Malformed filter: left-hand side of Contains clause must be e.g. TestCategory, was %O{key}"
        | ParsedFilter.String s -> failwith $"Malformed filter: got verbatim string %s{s} when expected an operation"

    /// Parse the input string, e.g. the `foo` one might get from `dotnet test --filter foo`.
    /// Verbatim strings are assumed to be XML-escaped.
    let parse (s : string) : Filter = ParsedFilter.parse s |> makeParsed

    /// Convert the representation of a test filter into a function that decides whether to run any given test.
    let rec shouldRun (filter : Filter) : TestFixture -> SingleTestMethod -> bool =
        match filter with
        | Filter.Not filter ->
            let inner = shouldRun filter
            fun a b -> not (inner a b)
        | Filter.And (a, b) ->
            let inner1 = shouldRun a
            let inner2 = shouldRun b
            fun a b -> inner1 a b && inner2 a b
        | Filter.Or (a, b) ->
            let inner1 = shouldRun a
            let inner2 = shouldRun b
            fun a b -> inner1 a b || inner2 a b
        | Filter.Name (Match.Exact m) -> fun _fixture method -> method.Method.Name = m
        | Filter.Name (Match.Contains m) -> fun _fixture method -> method.Method.Name.Contains m
        | Filter.FullyQualifiedName (Match.Exact m) -> fun fixture method -> (fixture.Name + method.Method.Name) = m
        | Filter.FullyQualifiedName (Match.Contains m) ->
            fun fixture method -> (fixture.Name + method.Method.Name).Contains m
        | Filter.TestCategory (Match.Contains m) ->
            fun _fixture method -> method.Categories |> List.exists (fun cat -> cat.Contains m)
        | Filter.TestCategory (Match.Exact m) -> fun _fixture method -> method.Categories |> List.contains m
