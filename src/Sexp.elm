module Sexp exposing (Datum(..), fmtNum, parse, toString)

import Parser exposing ((|.), (|=), Parser, Step(..))


type Datum
    = L (List Datum)
    | Sym String
    | Kw String
    | Str String
    | Num Float


parse : String -> Result String (List Datum)
parse src =
    Parser.run program src
        |> Result.mapError deadEndsToString


toString : Datum -> String
toString d =
    case d of
        L ds ->
            "(" ++ String.join " " (List.map toString ds) ++ ")"

        Sym s ->
            s

        Kw s ->
            ":" ++ s

        Str s ->
            s

        Num n ->
            fmtNum n


fmtNum : Float -> String
fmtNum n =
    if n == toFloat (round n) then
        String.fromInt (round n)

    else
        String.fromFloat n


program : Parser (List Datum)
program =
    Parser.succeed identity
        |. ws
        |= Parser.loop [] programStep
        |. Parser.end


programStep : List Datum -> Parser (Step (List Datum) (List Datum))
programStep acc =
    Parser.oneOf
        [ Parser.succeed (\d -> Loop (d :: acc))
            |= datum
            |. ws
        , Parser.succeed (Done (List.reverse acc))
        ]


datum : Parser Datum
datum =
    Parser.oneOf
        [ list
        , string
        , keyword
        , number
        , symbol
        ]


list : Parser Datum
list =
    Parser.succeed L
        |. Parser.symbol "("
        |. ws
        |= Parser.loop [] listStep


listStep : List Datum -> Parser (Step (List Datum) (List Datum))
listStep acc =
    Parser.oneOf
        [ Parser.succeed (Done (List.reverse acc))
            |. Parser.symbol ")"
        , Parser.succeed (\d -> Loop (d :: acc))
            |= Parser.lazy (\_ -> datum)
            |. ws
        ]


string : Parser Datum
string =
    Parser.succeed Str
        |. Parser.symbol "\""
        |= Parser.loop [] stringStep


stringStep : List String -> Parser (Step (List String) String)
stringStep acc =
    Parser.oneOf
        [ Parser.succeed (Done (String.concat (List.reverse acc)))
            |. Parser.symbol "\""
        , Parser.succeed (\c -> Loop (c :: acc))
            |. Parser.symbol "\\"
            |= escape
        , Parser.succeed (\s -> Loop (s :: acc))
            |= Parser.getChompedString (Parser.chompIf isPlain |. Parser.chompWhile isPlain)
        ]


isPlain : Char -> Bool
isPlain c =
    c /= '"' && c /= '\\'


escape : Parser String
escape =
    Parser.oneOf
        [ Parser.map (\_ -> "\"") (Parser.symbol "\"")
        , Parser.map (\_ -> "\\") (Parser.symbol "\\")
        , Parser.map (\_ -> "\n") (Parser.symbol "n")
        , Parser.map (\_ -> "\t") (Parser.symbol "t")
        ]


keyword : Parser Datum
keyword =
    Parser.succeed Kw
        |. Parser.symbol ":"
        |= Parser.getChompedString (Parser.chompIf isSymChar |. Parser.chompWhile isSymChar)


symbol : Parser Datum
symbol =
    Parser.succeed Sym
        |= Parser.getChompedString (Parser.chompIf isSymChar |. Parser.chompWhile isSymChar)


{-| 数値の直後にシンボル文字が続く場合(`1st` など)は数値ではなくシンボルとして扱うため、
末尾まで読んでから判定し、失敗時は backtrackable で symbol に譲る。
-}
number : Parser Datum
number =
    Parser.backtrackable
        (Parser.succeed Tuple.pair
            |= Parser.getChompedString
                (Parser.succeed ()
                    |. Parser.oneOf [ Parser.symbol "-", Parser.succeed () ]
                    |. Parser.chompIf Char.isDigit
                    |. Parser.chompWhile Char.isDigit
                    |. Parser.oneOf
                        [ Parser.symbol "."
                            |. Parser.chompIf Char.isDigit
                            |. Parser.chompWhile Char.isDigit
                        , Parser.succeed ()
                        ]
                )
            |= Parser.getChompedString (Parser.chompWhile isSymChar)
        )
        |> Parser.andThen toNumber


toNumber : ( String, String ) -> Parser Datum
toNumber ( digits, rest ) =
    if rest /= "" then
        Parser.problem "数値ではない"

    else
        case String.toFloat digits of
            Just n ->
                Parser.succeed (Num n)

            Nothing ->
                Parser.problem ("数値として読めない: " ++ digits)


ws : Parser ()
ws =
    Parser.loop () wsStep


wsStep : () -> Parser (Step () ())
wsStep _ =
    Parser.oneOf
        [ Parser.succeed (Loop ())
            |. Parser.chompIf isSpace
            |. Parser.chompWhile isSpace
        , Parser.succeed (Loop ())
            |. Parser.lineComment ";"
        , Parser.succeed (Done ())
        ]


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t' || c == ','


isSymChar : Char -> Bool
isSymChar c =
    not (isSpace c) && c /= '(' && c /= ')' && c /= '"' && c /= ';'


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.take 3
        |> List.map
            (\d ->
                String.fromInt d.row
                    ++ "行"
                    ++ String.fromInt d.col
                    ++ "列: "
                    ++ problemToString d.problem
            )
        |> String.join "\n"


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            s ++ " が必要"

        Parser.ExpectingSymbol s ->
            s ++ " が必要"

        Parser.ExpectingEnd ->
            "閉じ括弧が多い、または余分な入力"

        Parser.UnexpectedChar ->
            "予期しない文字"

        Parser.Problem s ->
            s

        _ ->
            "構文エラー"
