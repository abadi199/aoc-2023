app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-2-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    sum =
        input
        |> Str.split "\n"
        |> List.map parse
        |> List.keepOks (\a -> a)
        |> List.map power
        |> List.walk 0 Num.add

    Stdout.line
        (Num.toStr sum)

Game : {
    id : U64,
    sets : List GameSet,
}

GameSet : {
    r : U64,
    g : U64,
    b : U64,
}

power : Game -> U64
power = \game ->
    game.sets
    |> maximum
    |> \{ r, g, b } -> r * g * b

expect
    power {
        id: 1,
        sets: [
            { r: 4, g: 0, b: 3 },
            { r: 1, g: 2, b: 6 },
            { r: 0, g: 2, b: 0 },
        ],
    }
    == 48

maximum : List GameSet -> GameSet
maximum = \list ->
    list
    |> List.walk { r: 0, g: 0, b: 0 } \state, cube -> {
        r: Num.max state.r cube.r,
        g: Num.max state.g cube.g,
        b: Num.max state.b cube.b,

    }

expect
    maximum [
        { r: 4, g: 0, b: 3 },
        { r: 1, g: 2, b: 6 },
        { r: 0, g: 2, b: 0 },
    ]
    == { r: 4, g: 2, b: 6 }

isPossible : GameSet, Game -> Bool
isPossible = \bag, game ->
    game.sets
    |> List.map (\gameSet -> bag.r >= gameSet.r && bag.g >= gameSet.g && bag.b >= gameSet.b)
    |> List.walk Bool.true (Bool.and)

expect
    isPossible
        { r: 12, g: 13, b: 14 }
        { id: 1, sets: [{ b: 3, r: 4, g: 0 }] }
    == Bool.true
expect
    isPossible
        { r: 12, g: 13, b: 14 }
        { id: 1, sets: [{ b: 3, r: 14, g: 0 }] }
    == Bool.false
expect
    isPossible
        { r: 12, g: 13, b: 14 }
        {
            id: 1,
            sets: [
                { b: 6, r: 4, g: 5 },
                { b: 3, r: 14, g: 0 },
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
            { r: 4, g: 0, b: 3 },
            { r: 1, g: 2, b: 6 },
            { r: 0, g: 2, b: 0 },
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
        { r: 0, g: 0, b: 0 }
        \cube, cubeResult ->
            when cubeResult is
                Ok (Red count) -> { cube & r: cube.r + count }
                Ok (Green count) -> { cube & g: cube.g + count }
                Ok (Blue count) -> { cube & b: cube.b + count }
                _ -> cube

expect parseGameSet "3 blue, 4 red" == { r: 4, g: 0, b: 3 }

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
