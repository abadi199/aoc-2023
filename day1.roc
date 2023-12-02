app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-1-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    lines = Str.split input "\n"
    calibrationValues =
        lines
        |> List.map extractCalibrationValue

    sum =
        calibrationValues
        |> List.walk 0 Num.add

    Stdout.line
        (Num.toStr sum)

extractCalibrationValue : Str -> U64
extractCalibrationValue = \line ->
    line
    |> replaceWordWithNumber
    |> Str.graphemes
    |> List.walk [] \state, elem ->
        if isDigit elem then
            when state is
                [] -> [elem, elem]
                [first, ..] -> [first, elem]
        else
            state
    |> Str.joinWith ""
    |> Str.toU64
    |> Result.withDefault 0

expect extractCalibrationValue "1abc2" == 12
expect extractCalibrationValue "pqr3stu8vwx" == 38
expect extractCalibrationValue "a1b2c3d4e5f" == 15
expect extractCalibrationValue "treb7uchet" == 77

expect extractCalibrationValue "two1nine" == 29
expect extractCalibrationValue "eightwothree" == 83
expect extractCalibrationValue "abcone2threexyz" == 13
expect extractCalibrationValue "xtwone3four" == 24
expect extractCalibrationValue "4nineeightseven2" == 42
expect extractCalibrationValue "zoneight234" == 14
expect extractCalibrationValue "7pqrstsixteen" == 76

isDigit : Str -> Bool
isDigit = \value ->
    value
    |> Str.toU8
    |> Result.isOk

expect isDigit "5" == Bool.true
expect isDigit "a" == Bool.false

wordsToNumber = [
    ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9"),
    ("ten", "0"),
    ("eleven", "1"),
    ("twelve", "2"),
    ("thirteen", "3"),
    ("fourteen", "4"),
    ("fifteen", "5"),
    ("sixteen", "6"),
    ("seventeen", "7"),
    ("eighteen", "8"),
    ("nineteen", "9"),
    ("twenty", "0"),
    ("thirty", "0"),
    ("forty", "0"),
    ("fifty", "0"),
    ("sixty", "0"),
    ("seventy", "0"),
    ("eighty", "0"),
    ("ninety", "0"),
    ("hundred", "0"),
    ("thousand", "0"),
    ("million", "0"),
    ("billion", "0"),
]

replaceWordWithNumber : Str -> Str
replaceWordWithNumber = \value ->
    replacedWord =
        wordsToNumber
        |> List.walkUntil value \state, (word, number) ->
            if Str.startsWith value word then
                Break (Str.replaceFirst value word number)
            else
                Continue state

    graphemes = Str.graphemes replacedWord
    withoutFirstLetter =
        graphemes
        |> List.dropFirst 1
        |> Str.joinWith ""

    when graphemes is
        [] -> ""
        [firstLetter] -> firstLetter
        [firstLetter, ..] -> Str.concat firstLetter (replaceWordWithNumber withoutFirstLetter)



expect replaceWordWithNumber "two1nine" == "219"
expect
    output = replaceWordWithNumber "eightwothree"
    output == "8wo3"

expect
    output = replaceWordWithNumber "xtwone3four"
    output == "x2ne34"
