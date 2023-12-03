app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-3-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    result =
        input
        |> Str.split "\n"
        |> List.mapWithIndex parseLine
        |> combineSchemas
        |> filterParts
        |> sum
        |> Num.toStr

    Stdout.line result

Coord : (Nat, Nat)

Part : (U64, Coord)

Symbol : (Str, Coord)

Schema : { parts : List Part, symbols : List Symbol }

sum : Schema -> U64
sum = \{ parts } ->
    parts
    |> List.walk 0 \state, (partNumber, _) ->
        state + partNumber

filterParts : Schema -> Schema
filterParts = \schema -> {
    parts: schema.parts
    |> List.keepIf (\part -> isPart part schema.symbols),
    symbols: schema.symbols,
}

isPart : Part, List Symbol -> Bool
isPart = \part, symbols ->
    symbols
    |> List.walkUntil
        Bool.false
        (\_, symbol ->
            if isAdjecent part symbol then
                Break Bool.true
            else
                Continue Bool.false)

isAdjecent : Part, Symbol -> Bool
isAdjecent = \(number, (pX, pY)), (_, (sX, sY)) ->
    numberLength =
        number
        |> Num.toStr
        |> Str.graphemes
        |> List.len

    r1 = { x1: pX, y1: pY, x2: pX + numberLength - 1, y2: pY }
    r2 = {
        x1: if sX > 0 then sX - 1 else 0,
        y1: if sY > 0 then sY - 1 else 0,
        x2: sX + 1,
        y2: sY + 1,
    }

    noOverlap =
        (r1.x1 > r2.x2)
        || (r2.x1 > r1.x2)
        || (r1.y1 > r2.y2)
        || (r2.y1 > r1.y2)

    !noOverlap

expect
    isAdjecent
        (467, (0, 0))
        ("*", (3, 1))
    == Bool.true

expect
    isAdjecent
        (114, (5, 0))
        ("*", (3, 1))
    == Bool.false

combineSchemas : List Schema -> Schema
combineSchemas = \list ->
    list
    |> List.walk { parts: [], symbols: [] } \state, schema -> {
        parts: List.concat state.parts schema.parts,
        symbols: List.concat state.symbols schema.symbols,
    }

parseLine : Str, Nat -> Schema
parseLine = \value, y -> {
    parts: parseParts value y,
    symbols: parseSymbols value y,
}

parseParts : Str, Nat -> List Part
parseParts = \value, y ->
    value
    |> partParser
    |> List.keepOks (\r -> toPart r y)

expect parseParts "467..114.." 0 == [(467, (0, 0)), (114, (5, 0))]
expect parseParts "...*......" 1 == []
expect parseParts "..35..633." 2 == [(35, (2, 2)), (633, (6, 2))]

parseSymbols : Str, Nat -> List Symbol
parseSymbols = \value, y ->
    value
    |> Str.graphemes
    |> List.walkWithIndex [] \symbols, g, x ->
        if isDigit g || g == "." then
            symbols
        else
            List.append symbols ((g, (x, y)))

expect parseSymbols "...*......" 1 == [("*", (3, 1))]
expect parseSymbols "...$.*...." 8 == [("$", (3, 8)), ("*", (5, 8))]

isDigit : Str -> Bool
isDigit = \value ->
    ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    |> List.contains value

ParserOutput : [Num (List Str) Nat, Skip]

partParser : Str -> List ParserOutput
partParser = \value ->
    value
    |> Str.graphemes
    |> List.walkWithIndex [] \state, grapheme, x ->
        if isDigit grapheme then
            when state is
                [] | [.., Skip] ->
                    List.append
                        state
                        (Num [grapheme] (x))

                [.., Num list xCoord] ->
                    List.append
                        (List.dropLast state 1)
                        (Num (List.append list grapheme) xCoord)
        else
            List.append state Skip

expect
    partParser "467"
    == [
        Num ["4", "6", "7"] 0,
    ]

expect
    partParser "467..114.."
    == [
        Num ["4", "6", "7"] 0,
        Skip,
        Skip,
        Num ["1", "1", "4"] 5,
        Skip,
        Skip,
    ]

toPart : ParserOutput, Nat -> Result Part [Error]
toPart = \value, y ->
    when value is
        Num list x ->
            list
            |> Str.joinWith ""
            |> Str.toU64
            |> Result.map (\num -> (num, (x, y)))
            |> Result.mapErr (\_ -> Error)

        Skip -> Err Error
