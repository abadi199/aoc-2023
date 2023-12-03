app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-2-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    bag =
        { red: 12, green: 13, blue: 14 }

    possibleGames =
        input
        |> Str.split "\n"
        |> List.map parse
        |> List.keepOks (\a -> a)
        |> List.keepIf
            (\game -> isPossible bag game)

    sum =
        possibleGames
        |> List.map .id
        |> List.walk 0 Num.add

    Stdout.line
        (Num.toStr sum)

Game : {
    id : U64,
    sets : List GameSet,
}

GameSet : {
    red : U64,
    green : U64,
    blue : U64,
}

isPossible : GameSet, Game -> Bool
isPossible = \bag, game ->
    game.sets
    |> List.map (\gameSet -> bag.red >= gameSet.red && bag.green >= gameSet.green && bag.blue >= gameSet.blue)
    |> List.walk Bool.true (Bool.and)

expect
    isPossible
        { red: 12, green: 13, blue: 14 }
        { id: 1, sets: [{ blue: 3, red: 4, green: 0 }] }
    == Bool.true
expect
    isPossible
        { red: 12, green: 13, blue: 14 }
        { id: 1, sets: [{ blue: 3, red: 14, green: 0 }] }
    == Bool.false
expect
    isPossible
        { red: 12, green: 13, blue: 14 }
        {
            id: 1,
            sets: [
                { blue: 6, red: 4, green: 5 },
                { blue: 3, red: 14, green: 0 },
            ],
        }
    == Bool.false

parse : Str -> Result Game [Error]
parse = \value ->
    when Str.split value ":" is
        [gameStr, gameSetsStr] ->
            sets = parseGameSets gameSetsStr

            parseGameId gameStr
            |> Result.map \id -> {
                id,
                sets,
            }

        _ -> Err Error

expect
    parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    == Ok {
        id: 1,
        sets: [
            { red: 4, green: 0, blue: 3 },
            { red: 1, green: 2, blue: 6 },
            { red: 0, green: 2, blue: 0 },
        ],
    }

parseGameId : Str -> Result U64 [Error]
parseGameId = \value ->
    Str.replaceFirst value "Game " ""
    |> Str.toU64
    |> Result.mapErr \_ -> Error

expect parseGameId "Game 88" == Ok 88

parseGameSets : Str -> List GameSet
parseGameSets = \value ->
    value
    |> Str.split ";"
    |> List.map parseGameSet

parseGameSet : Str -> GameSet
parseGameSet = \value ->
    value
    |> Str.split ","
    |> List.map Str.trim
    |> List.map parseCube
    |> List.walk
        { red: 0, green: 0, blue: 0 }
        \cube, cubeResult ->
            when cubeResult is
                Ok (Red count) -> { cube & red: cube.red + count }
                Ok (Green count) -> { cube & green: cube.green + count }
                Ok (Blue count) -> { cube & blue: cube.blue + count }
                _ -> cube

expect parseGameSet "3 blue, 4 red" == { red: 4, green: 0, blue: 3 }

Cube : [Red U64, Green U64, Blue U64]

parseCube : Str -> Result Cube [Error]
parseCube = \value ->
    count =
        value
        |> Str.split " "
        |> List.first
        |> Result.map Str.trim
        |> Result.try Str.toU64
        |> Result.mapErr \_ -> Error

    if Str.endsWith value "red" then
        count
        |> Result.map Red
    else if Str.endsWith value "green" then
        count
        |> Result.map Green
    else if Str.endsWith value "blue" then
        count
        |> Result.map Blue
    else
        Err Error

expect parseCube "3 blue" == Ok (Blue 3)

map2 : Result a err, Result b err, (a, b -> c) -> Result c err
map2 = \a, b, f ->
    when (a, b) is
        (Ok aa, Ok bb) -> Ok (f aa bb)
        (Err e, _) -> Err e
        (_, Err e) -> Err e

join : List (Result a e) -> Result (List a) e
join = \list ->
    list
    |> List.walk (Ok []) \state, result ->
        when result is
            Ok a -> state |> Result.map (\listState -> List.append listState a)
            Err e -> Err e
