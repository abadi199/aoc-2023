app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-3-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    Stdout.line ""

Coord : (U64, U64)

Part : (U64, Coord)

Symbol : (Str, Coord)

Schema : { parts : List Part, symbols : List Symbol }

parseLine : U64, Str -> Schema
parseLine = \y, value -> {
    parts: parseParts y value,
    symbols: [],
}

parseParts : U64, Str -> List Part
parseParts = \y, value ->
    value
    |> parser
    |> List.keepOks (\r -> toPart y r)

expect parseParts 0 "467..114.." == [(467, (0, 0)), (114, (5, 0))]
expect parseParts 1 "...*......" == []
expect parseParts 2 "..35..633." == [(35, (2, 2)), (633, (6, 2))]

isDigit : Str -> Bool
isDigit = \value ->
    ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    |> List.contains value

ParserOutput : [Num (List Str) U64, Skip]

parser : Str -> List ParserOutput
parser = \value ->
    value
    |> Str.graphemes
    |> List.walkWithIndex [] \state, grapheme, x ->
        if isDigit grapheme then
            when state is
                [] | [.., Skip] ->
                    List.append
                        state
                        (Num [grapheme] (Num.toU64 x))

                [.., Num list xCoord] ->
                    List.append
                        (List.dropLast state 1)
                        (Num (List.append list grapheme) xCoord)
        else
            List.append state Skip

expect
    parser "467"
    == [
        Num ["4", "6", "7"] 0,
    ]

expect
    o = parser "467..114.."
    o
    == [
        Num ["4", "6", "7"] 0,
        Skip,
        Skip,
        Num ["1", "1", "4"] 5,
        Skip,
        Skip,
    ]

toPart : U64, ParserOutput -> Result Part [Error]
toPart = \y, value ->
    when value is
        Num list x ->
            list
            |> Str.joinWith ""
            |> Str.toU64
            |> Result.map (\num -> (num, (x, y)))
            |> Result.mapErr (\_ -> Error)

        Skip -> Err Error
