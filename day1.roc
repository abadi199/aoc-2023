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
    Str.graphemes line
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

isDigit : Str -> Bool
isDigit = \value ->
    value
    |> Str.toU8
    |> Result.isOk

expect isDigit "5" == Bool.true
expect isDigit "a" == Bool.false
