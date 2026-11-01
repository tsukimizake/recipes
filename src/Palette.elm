module Palette exposing (background, phase)

import Color
import Css


background : Css.Color
background =
    Css.hex "fcfcfb"


phase : Int -> Css.Color
phase index =
    let
        colors =
            [ Css.hex "2a78d6"
            , Css.hex "eb6834"
            , Css.hex "1baf7a"
            , Css.hex "eda100"
            , Css.hex "e87ba4"
            , Css.hex "008300"
            , Css.hex "4a3aa7"
            , Css.hex "e34948"
            ]
    in
    List.drop (modBy (List.length colors) index) colors
        |> List.head
        |> Maybe.withDefault Color.secondary
