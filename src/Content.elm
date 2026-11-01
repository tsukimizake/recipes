module Content exposing (Content, actionLH, actionSize, contentFor, contentHeight, layoutConfig, nodeW, padX, padY, sectionGap, smallLH, smallSize)

import Layout
import Rcp exposing (Node)


type alias Content =
    { ingredients : List String
    , action : List String
    , tools : List String
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


actionSize : Float
actionSize =
    14


smallSize : Float
smallSize =
    11.5


actionLH : Float
actionLH =
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
        smallUnits =
            (nodeW - 2 * padX) / smallSize
    in
    Content
        (List.concatMap
            (\item ->
                case item of
                    Rcp.IngItem ing ->
                        wrap smallUnits
                            ("・"
                                ++ ing.name
                                ++ (case ( ing.qty, ing.unit ) of
                                        ( Just qty, Just unit ) ->
                                            " " ++ qty ++ " " ++ unit

                                        _ ->
                                            ""
                                   )
                                ++ (case ing.note of
                                        Just note ->
                                            "（" ++ note ++ "）"

                                        Nothing ->
                                            ""
                                   )
                            )

                    Rcp.ToolItem _ ->
                        []
            )
            node.items
        )
        (wrap ((nodeW - 2 * padX) / actionSize) node.action)
        (List.concatMap
            (\item ->
                case item of
                    Rcp.ToolItem name ->
                        wrap smallUnits ("▢ " ++ name)

                    Rcp.IngItem _ ->
                        []
            )
            node.items
        )
        (List.filterMap identity
            [ Maybe.map (\time -> "⏱ " ++ time) node.annots.time
            , Maybe.map
                (\attention ->
                    case attention of
                        "full" ->
                            "かかりきり"

                        "partial" ->
                            "ときどき"

                        "none" ->
                            "放置"

                        _ ->
                            attention
                )
                node.annots.attention
            , Maybe.map
                (\heat ->
                    if heat == "on" then
                        "🔥"

                    else
                        "🔥 " ++ heat
                )
                node.annots.heat
            , Maybe.map (\tool -> "🔧 " ++ tool) node.annots.tool
            ]
            |> String.join "\u{3000}"
            |> wrap smallUnits
        )
        (List.filterMap identity
            [ Maybe.map (\until -> "→ " ++ until) node.annots.until
            , Maybe.map (\note -> "※ " ++ note) node.annots.note
            ]
            |> List.concatMap (wrap smallUnits)
        )


contentHeight : Content -> Float
contentHeight c =
    let
        sections =
            [ ( smallLH, c.ingredients )
            , ( actionLH, c.action )
            , ( smallLH, c.tools )
            , ( smallLH, c.badges )
            , ( smallLH, c.detail )
            ]
                |> List.filter (Tuple.second >> List.isEmpty >> not)
    in
    padY
        * 2
        + List.sum (List.map (\( lineHeight, lines ) -> lineHeight * toFloat (List.length lines)) sections)
        + sectionGap
        * toFloat (max 0 (List.length sections - 1))


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
