module Content exposing (Content, contentFor, contentHeight, labelLH, labelSize, layoutConfig, nodeW, padX, padY, sectionGap, smallLH, smallSize)

import Layout
import Rcp exposing (Node)


type alias Content =
    { label : List String
    , items : List String
    , badges : List String
    , detail : List String
    }


nodeW : Float
nodeW =
    224


padX : Float
padX =
    14


padY : Float
padY =
    10


labelSize : Float
labelSize =
    14


smallSize : Float
smallSize =
    11.5


labelLH : Float
labelLH =
    20


smallLH : Float
smallLH =
    16


sectionGap : Float
sectionGap =
    5


layoutConfig : Layout.Config
layoutConfig =
    { nodeW = nodeW, gapX = 48, gapY = 60, margin = 24, dummyW = 0.35, finalR = 10 }


contentFor : Node -> Content
contentFor node =
    let
        labelUnits =
            (nodeW - 2 * padX) / labelSize

        smallUnits =
            (nodeW - 2 * padX) / smallSize

        a =
            node.annots

        badges =
            List.filterMap identity
                [ Maybe.map (\t -> "⏱ " ++ t) a.time
                , Maybe.map attentionText a.attention
                , Maybe.map heatText a.heat
                , Maybe.map (\t -> "🔧 " ++ t) a.tool
                ]
    in
    { label = wrap labelUnits node.label
    , items = List.concatMap (itemText >> wrap smallUnits) node.items
    , badges =
        if List.isEmpty badges then
            []

        else
            wrap smallUnits (String.join "\u{3000}" badges)
    , detail =
        List.filterMap identity
            [ Maybe.map (\u -> "→ " ++ u) a.until
            , Maybe.map (\n -> "※ " ++ n) a.note
            ]
            |> List.concatMap (wrap smallUnits)
    }


attentionText : String -> String
attentionText s =
    case s of
        "full" ->
            "かかりきり"

        "partial" ->
            "ときどき"

        "none" ->
            "放置"

        _ ->
            s


heatText : String -> String
heatText s =
    if s == "on" then
        "🔥"

    else
        "🔥 " ++ s


itemText : Rcp.Item -> String
itemText item =
    case item of
        Rcp.ToolItem name ->
            "▢ " ++ name

        Rcp.IngItem ing ->
            let
                qty =
                    case ( ing.qty, ing.unit ) of
                        ( Just q, Just u ) ->
                            " " ++ q ++ " " ++ u

                        _ ->
                            ""

                note =
                    case ing.note of
                        Just n ->
                            "（" ++ n ++ "）"

                        Nothing ->
                            ""
            in
            "・" ++ ing.name ++ qty ++ note


contentHeight : Content -> Float
contentHeight c =
    let
        section lines =
            if List.isEmpty lines then
                0

            else
                sectionGap + smallLH * toFloat (List.length lines)
    in
    padY * 2 + labelLH * toFloat (List.length c.label) + section c.items + section c.badges + section c.detail


{-| 全角を1、それ以外を0.55単位として貪欲に折り返す。
-}
wrap : Float -> String -> List String
wrap maxUnits s =
    let
        step c ( lines, cur, used ) =
            let
                u =
                    charUnits c
            in
            if used + u > maxUnits && not (List.isEmpty cur) then
                ( String.fromList (List.reverse cur) :: lines, [ c ], u )

            else
                ( lines, c :: cur, used + u )

        ( done, last, _ ) =
            List.foldl step ( [], [], 0 ) (String.toList s)
    in
    List.reverse
        (if List.isEmpty last then
            done

         else
            String.fromList (List.reverse last) :: done
        )


charUnits : Char -> Float
charUnits c =
    if Char.toCode c >= 0x2000 then
        1

    else
        0.55
