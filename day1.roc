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
    Str.concat (firstDigit line LeftToRight) (firstDigit line RightToLeft)
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

expect extractCalibrationValue "2threesixqczmrxhdrsevenfouroneight" == 28

Direction : [LeftToRight, RightToLeft]

firstDigit : Str, Direction -> Str
firstDigit = \value, direction ->
    result =
        value
        |> parseDigitNumber direction
        |> Result.onErr (\_ -> parseDigitWord value direction)

    nextValue =
        dropFirst value direction

    when result is
        Ok digit -> digit
        Err _ ->
            if nextValue == "" then
                "0"
            else
                firstDigit nextValue direction

expect firstDigit "1two" LeftToRight == "1"
expect firstDigit "twone3" LeftToRight == "2"
expect firstDigit "xtwone3" LeftToRight == "2"
expect firstDigit "xtwelvene3" LeftToRight == "1"

expect firstDigit "7pqrstsixteen" RightToLeft == "6"

expect parseDigitNumber "1one2" LeftToRight == Ok "1"
expect parseDigitNumber "one" LeftToRight == Err Error

expect parseDigitWord "oneight" LeftToRight == Ok "1"
expect parseDigitWord "twenty" LeftToRight == Ok "2"

expect parseDigitNumber "1two3" RightToLeft == Ok "3"
expect parseDigitNumber "one" RightToLeft == Err Error

expect parseDigitWord "oneight" RightToLeft == Ok "8"
expect parseDigitWord "twenty" RightToLeft == Ok "0"

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
    ("ten", "10"),
    ("eleven", "11"),
    ("twelve", "12"),
    ("thirteen", "13"),
    ("fourteen", "14"),
    ("fifteen", "15"),
    ("sixteen", "16"),
    ("seventeen", "17"),
    ("eighteen", "18"),
    ("nineteen", "19"),
    ("twenty", "20"),
    ("thirty", "30"),
    ("forty", "40"),
    ("fifty", "50"),
    ("sixty", "60"),
    ("seventy", "70"),
    ("eighty", "80"),
    ("ninety", "90"),
]

reverse : Str -> Str
reverse = \value ->
    value
    |> Str.graphemes
    |> List.reverse
    |> Str.joinWith ""

dropFirst : Str, Direction -> Str
dropFirst = \value, direction ->
    value
    |> Str.graphemes
    |> \list ->
        when direction is
            LeftToRight ->
                List.dropFirst list 1

            RightToLeft ->
                List.dropLast list 1
    |> Str.joinWith ""

parseDigitNumber : Str, Direction -> Result Str [Error]
parseDigitNumber = \value, direction ->
    value
    |> Str.graphemes
    |> \graphemes ->
        when direction is
            LeftToRight -> List.first graphemes
            RightToLeft -> List.last graphemes
    |> Result.try Str.toU8
    |> Result.map Num.toStr
    |> Result.mapErr \_ -> Error

parseDigitWord : Str, Direction -> Result Str [Error]
parseDigitWord = \value, direction ->
    directedValue =
        when direction is
            LeftToRight -> value
            RightToLeft -> reverse value

    wordsToNumber
    |> List.walkUntil (Err Error) \_, (word, number) ->
        when direction is
            LeftToRight ->
                if Str.startsWith directedValue word then
                    Break (Ok number)
                else
                    Continue (Err Error)

            RightToLeft ->
                if Str.startsWith directedValue (reverse word) then
                    Break (Ok (reverse number))
                else
                    Continue (Err Error)
    |> Result.try (\digit -> digit |> Str.graphemes |> List.first)
    |> Result.mapErr (\_ -> Error)
