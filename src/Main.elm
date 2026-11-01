module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Color
import Content exposing (Content, contentFor, contentHeight, labelLH, labelSize, layoutConfig, padX, padY, sectionGap, smallLH, smallSize)
import Css
import Css.Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, h1, li, span, text, ul)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as HE
import Http
import Json.Decode as D
import Layout exposing (Layout)
import Palette
import Rcp exposing (Graph, Node, Recipe)
import Set exposing (Set)
import Sexp
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SA
import Svg.Styled.Events as SE
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , index : Index
    , recipeName : Maybe String
    , state : State
    , done : Set Int
    }


type Index
    = IndexLoading
    | IndexFailed String
    | IndexLoaded (List String)


type State
    = Idle
    | Loading
    | Failed String
    | Loaded Prepared


type alias Prepared =
    { recipe : Recipe
    , graph : Graph
    , contents : Dict Int Content
    , layout : Layout
    , phases : List String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotIndex (Result Http.Error (List String))
    | GotRecipe (Result Http.Error String)
    | ToggleDone Int
    | SelectRecipe String


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        params =
            paramsFromUrl url
    in
    ( { key = key
      , index = IndexLoading
      , recipeName = params.recipe
      , state = stateFor params.recipe
      , done = params.done
      }
    , Cmd.batch [ loadIndex, Maybe.map load params.recipe |> Maybe.withDefault Cmd.none ]
    )


stateFor : Maybe String -> State
stateFor recipe =
    case recipe of
        Just _ ->
            Loading

        Nothing ->
            Idle


type alias Params =
    { recipe : Maybe String
    , done : Set Int
    }


{-| 完了状態は URL の `done=1,2,3` に保持し、リロードしても残るようにする。
-}
paramsFromUrl : Url -> Params
paramsFromUrl url =
    let
        query =
            Url.Parser.Query.map2 Tuple.pair
                (Url.Parser.Query.string "recipe")
                (Url.Parser.Query.string "done")
    in
    case Url.Parser.parse (Url.Parser.query query) { url | path = "/" } of
        Just ( name, done ) ->
            { recipe = name
            , done =
                done
                    |> Maybe.withDefault ""
                    |> String.split ","
                    |> List.filterMap String.toInt
                    |> Set.fromList
            }

        Nothing ->
            { recipe = Nothing, done = Set.empty }


queryFor : String -> Set Int -> String
queryFor name done =
    "?recipe="
        ++ Url.percentEncode name
        ++ (if Set.isEmpty done then
                ""

            else
                "&done=" ++ String.join "," (List.map String.fromInt (Set.toList done))
           )


{-| `recipes.json` は `just build` が生成する `recipes/` 内のファイル名の配列。
-}
loadIndex : Cmd Msg
loadIndex =
    Http.get
        { url = "recipes.json"
        , expect =
            Http.expectJson GotIndex
                (D.list D.string)
        }


load : String -> Cmd Msg
load name =
    Http.get { url = "recipes/" ++ name, expect = Http.expectString GotRecipe }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                params =
                    paramsFromUrl url
            in
            if params.recipe == model.recipeName then
                ( { model | done = params.done }, Cmd.none )

            else
                ( { model | recipeName = params.recipe, state = stateFor params.recipe, done = params.done }
                , Maybe.map load params.recipe |> Maybe.withDefault Cmd.none
                )

        GotIndex (Ok entries) ->
            ( { model | index = IndexLoaded entries }
            , case ( model.recipeName, entries ) of
                ( Nothing, first :: _ ) ->
                    Nav.replaceUrl model.key (queryFor first Set.empty)

                _ ->
                    Cmd.none
            )

        GotIndex (Err err) ->
            ( { model | index = IndexFailed (httpError "recipes.json" err) }, Cmd.none )

        GotRecipe (Ok src) ->
            ( { model | state = prepare src }, Cmd.none )

        GotRecipe (Err err) ->
            ( { model | state = Failed (httpError (currentName model) err) }, Cmd.none )

        ToggleDone id ->
            setDone
                (if Set.member id model.done then
                    Set.remove id model.done

                 else
                    Set.insert id model.done
                )
                model

        SelectRecipe file ->
            ( model, Nav.pushUrl model.key (queryFor file Set.empty) )


setDone : Set Int -> Model -> ( Model, Cmd Msg )
setDone done model =
    case model.recipeName of
        Just name ->
            ( { model | done = done }, Nav.replaceUrl model.key (queryFor name done) )

        Nothing ->
            ( model, Cmd.none )


displayName : String -> String
displayName file =
    if String.endsWith ".rcp" file then
        String.dropRight 4 file

    else
        file


currentName : Model -> String
currentName model =
    Maybe.withDefault "レシピ" model.recipeName


httpError : String -> Http.Error -> String
httpError name err =
    case err of
        Http.BadUrl u ->
            "URLが不正: " ++ u

        Http.Timeout ->
            "タイムアウト"

        Http.NetworkError ->
            "ネットワークエラー"

        Http.BadStatus code ->
            name ++ " の取得に失敗 (HTTP " ++ String.fromInt code ++ ")"

        Http.BadBody body ->
            body


prepare : String -> State
prepare src =
    let
        parsed =
            Sexp.parse src
                |> Result.andThen Rcp.fromDatums
                |> Result.andThen (\r -> Rcp.buildGraph r |> Result.map (Tuple.pair r))
    in
    case parsed of
        Err e ->
            Failed e

        Ok ( recipe, graph ) ->
            let
                contents =
                    graph.nodes
                        |> List.map (\n -> ( n.id, contentFor n ))
                        |> Dict.fromList

                heights =
                    Dict.map (\_ c -> contentHeight c) contents
            in
            Loaded
                { recipe = recipe
                , graph = graph
                , contents = contents
                , layout = Layout.compute layoutConfig (List.map .id graph.nodes) heights graph.edges graph.result
                , phases = graph.nodes |> List.filterMap (.annots >> .phase) |> unique
                }


unique : List String -> List String
unique xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        xs



-- 表示


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.state of
            Loaded p ->
                Maybe.withDefault (currentName model) (Rcp.metaGet "title" p.recipe)

            _ ->
                currentName model
    , body =
        List.map Html.toUnstyled
            [ Css.Global.global
                [ Css.Global.body
                    [ Css.margin Css.zero
                    , Css.fontFamilies [ "-apple-system", Css.qt "Hiragino Sans", Css.qt "Noto Sans JP", "sans-serif" ]
                    , Css.color Color.primary
                    , Css.backgroundColor Palette.background
                    ]
                ]
            , div [ Attr.css [ Css.width (Css.pct 100) ] ]
                [ viewHeader model
                , div [] [ viewMain model ]
                ]
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Html.header
        [ Attr.css
            [ Css.position Css.sticky
            , Css.top Css.zero
            , Css.left Css.zero
            , Css.zIndex (Css.int 1)
            , Css.boxSizing Css.borderBox
            , Css.width (Css.pct 100)
            , Css.maxWidth (Css.vw 100)
            , Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.alignItems Css.center
            , Css.property "gap" "4px 14px"
            , Css.padding2 (Css.px 6) (Css.px 12)
            , Css.backgroundColor Palette.background
            , Css.borderBottom3 (Css.px 1) Css.solid Color.border
            , Css.fontSize (Css.px 12)
            ]
        ]
        ([ h1 [ Attr.css [ Css.fontSize (Css.px 15), Css.margin Css.zero ] ]
            [ text
                (case model.state of
                    Loaded p ->
                        Maybe.withDefault (currentName model) (Rcp.metaGet "title" p.recipe)

                    _ ->
                        currentName model
                )
            ]
         ]
            ++ (case model.state of
                    Loaded p ->
                        [ span [ Attr.css [ Css.color Color.secondary ] ] [ text (metaLine p.recipe) ] ]
                            ++ (if List.isEmpty p.recipe.notes then
                                    []

                                else
                                    [ Html.details
                                        [ Attr.css [ Css.Global.withAttribute "open" [ Css.flexBasis (Css.pct 100) ] ] ]
                                        [ Html.summary [ Attr.css [ Css.cursor Css.pointer, Css.color Color.secondary ] ]
                                            [ text ("メモ " ++ String.fromInt (List.length p.recipe.notes) ++ "件") ]
                                        , ul
                                            [ Attr.css
                                                [ Css.margin3 (Css.px 4) Css.zero Css.zero
                                                , Css.paddingLeft (Css.em 1.2)
                                                , Css.color Color.secondary
                                                ]
                                            ]
                                            (List.map (\n -> li [] [ text n ]) p.recipe.notes)
                                        ]
                                    ]
                               )
                            ++ [ div
                                    [ Attr.css [ Css.displayFlex, Css.property "gap" "10px", Css.flexWrap Css.wrap ] ]
                                    (List.indexedMap legendItem p.phases)
                               , span []
                                    [ text
                                        (String.fromInt (List.length (List.filter (\n -> Set.member n.id model.done) p.graph.nodes))
                                            ++ " / "
                                            ++ String.fromInt (List.length p.graph.nodes)
                                            ++ " 完了"
                                        )
                                    ]
                               ]

                    _ ->
                        []
               )
            ++ [ case model.index of
                    IndexLoaded entries ->
                        Html.select [ HE.onInput SelectRecipe, Attr.css [ Css.property "font" "inherit" ] ]
                            (List.map
                                (\file ->
                                    Html.option
                                        [ Attr.value file, Attr.selected (Just file == model.recipeName) ]
                                        [ text (displayName file) ]
                                )
                                entries
                            )

                    IndexLoading ->
                        text ""

                    IndexFailed e ->
                        span [ Attr.css [ Css.color Color.danger ] ]
                            [ text ("レシピ一覧を読めません (" ++ e ++ ")。just build で recipes.json を生成してください") ]
               ]
        )


metaLine : Recipe -> String
metaLine recipe =
    List.filterMap identity
        [ Rcp.metaGet "servings" recipe |> Maybe.map (\s -> s ++ "人分")
        , Rcp.metaGet "prep-time" recipe |> Maybe.map (\s -> "下準備 " ++ s)
        , Rcp.metaGet "cook-time" recipe |> Maybe.map (\s -> "調理 " ++ s)
        , Rcp.metaGet "time-required" recipe |> Maybe.map (\s -> "合計 " ++ s)
        , Rcp.metaGet "author" recipe
        , Rcp.metaGet "source" recipe
        ]
        |> String.join " · "


legendItem : Int -> String -> Html Msg
legendItem i phase =
    span [ Attr.css [ Css.display Css.inlineFlex, Css.alignItems Css.center, Css.property "gap" "4px" ] ]
        [ span
            [ Attr.css
                [ Css.width (Css.px 10)
                , Css.height (Css.px 10)
                , Css.borderRadius (Css.pct 50)
                , Css.display Css.inlineBlock
                , Css.backgroundColor (Palette.phase i)
                ]
            ]
            []
        , text phase
        ]


viewMain : Model -> Html Msg
viewMain model =
    case model.state of
        Idle ->
            case model.index of
                IndexLoaded [] ->
                    div [ Attr.css [ Css.padding (Css.px 20), Css.color Color.danger, Css.whiteSpace Css.preWrap ] ]
                        [ text "recipes/ に .rcp ファイルがありません" ]

                IndexFailed e ->
                    div [ Attr.css [ Css.padding (Css.px 20), Css.color Color.danger, Css.whiteSpace Css.preWrap ] ] [ text e ]

                _ ->
                    div [ Attr.css [ Css.padding (Css.px 20), Css.color Color.secondary ] ] [ text "読み込み中…" ]

        Loading ->
            div [ Attr.css [ Css.padding (Css.px 20), Css.color Color.secondary ] ] [ text "読み込み中…" ]

        Failed e ->
            div [ Attr.css [ Css.padding (Css.px 20), Css.color Color.danger, Css.whiteSpace Css.preWrap ] ] [ text e ]

        Loaded p ->
            viewSvg model.done p


phaseColor : List String -> Maybe String -> String
phaseColor phases phase =
    case phase of
        Nothing ->
            "#7a7975"

        Just ph ->
            phases
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, p ) -> p == ph)
                |> List.head
                |> Maybe.map (Tuple.first >> Palette.phase >> .value)
                |> Maybe.withDefault "#7a7975"


inkDim : String
inkDim =
    "#8a8985"


inkPrimary : String
inkPrimary =
    Color.primary.value


inkSecondary : String
inkSecondary =
    Color.secondary.value


strokeDim : String
strokeDim =
    "#c3c2b7"


viewSvg : Set Int -> Prepared -> Html Msg
viewSvg done p =
    let
        outDeg =
            degree .from p.layout.edges

        inDeg =
            degree .to p.layout.edges
    in
    Svg.svg
        [ SA.width (f p.layout.width)
        , SA.height (f p.layout.height)
        , SA.viewBox ("0 0 " ++ f p.layout.width ++ " " ++ f p.layout.height)
        , SA.fontFamily "-apple-system, 'Hiragino Sans', 'Noto Sans JP', sans-serif"
        , SA.css
            [ Css.display Css.block
            , Css.width (Css.pct 100)
            , Css.maxWidth (Css.px p.layout.width)
            , Css.height Css.auto
            ]
        ]
        (defs
            :: List.map (viewEdgePath done) p.layout.edges
            ++ List.map (viewNode done p) p.graph.nodes
            ++ List.filterMap (viewEdgeLabel done outDeg inDeg) p.layout.edges
            ++ viewFinal p.layout.final
        )


degree : (Layout.PlacedEdge -> Int) -> List Layout.PlacedEdge -> Dict Int Int
degree endpoint edges =
    List.foldl (\e d -> Dict.update (endpoint e) (\m -> Just (1 + Maybe.withDefault 0 m)) d) Dict.empty edges


defs : Svg Msg
defs =
    Svg.defs []
        [ arrowMarker "arrow" inkSecondary
        , arrowMarker "arrow-dim" strokeDim
        ]


arrowMarker : String -> String -> Svg Msg
arrowMarker id color =
    Svg.marker
        [ SA.id id
        , SA.viewBox "0 0 10 10"
        , SA.refX "10"
        , SA.refY "5"
        , SA.markerWidth "7"
        , SA.markerHeight "7"
        , SA.orient "auto-start-reverse"
        ]
        [ Svg.path [ SA.d "M 0 0 L 10 5 L 0 10 z", SA.fill color ] [] ]


viewEdgePath : Set Int -> Layout.PlacedEdge -> Svg Msg
viewEdgePath done e =
    let
        dim =
            Set.member e.from done
    in
    Svg.path
        [ SA.d (pathD e.points)
        , SA.fill "none"
        , SA.stroke
            (if dim then
                strokeDim

             else
                inkSecondary
            )
        , SA.strokeWidth "1.5"
        , SA.markerEnd
            (if dim then
                "url(#arrow-dim)"

             else
                "url(#arrow)"
            )
        ]
        []


{-| 分岐(出次数2以上)の辺は終点側、合流(入次数2以上)の辺は始点側にラベルを置く。
辺が束になる側を避けるほうが読みやすい。
-}
viewEdgeLabel : Set Int -> Dict Int Int -> Dict Int Int -> Layout.PlacedEdge -> Maybe (Svg Msg)
viewEdgeLabel done outDeg inDeg e =
    let
        position =
            if Maybe.withDefault 0 (Dict.get e.from outDeg) > 1 then
                nearTarget e.points

            else if Maybe.withDefault 0 (Dict.get e.to inDeg) > 1 then
                nearSource e.points

            else
                midpoint e.points
    in
    Maybe.map2
        (\label ( x, y ) ->
            Svg.text_
                [ SA.x (f x)
                , SA.y (f (y + 4))
                , SA.textAnchor "middle"
                , SA.fontSize "11"
                , SA.fill
                    (if Set.member e.from done then
                        inkDim

                     else
                        inkSecondary
                    )
                , SA.stroke "#fcfcfb"
                , SA.strokeWidth "3"
                , SA.strokeLinejoin "round"
                , Attr.attribute "paint-order" "stroke"
                ]
                [ Svg.text label ]
        )
        e.label
        position


{-| 各区間を上下端で垂直な接線を持つ3次ベジェで結ぶ。ダミーノードを通る辺も滑らかにつながる。
-}
pathD : List ( Float, Float ) -> String
pathD points =
    case points of
        ( x0, y0 ) :: rest ->
            "M" ++ f x0 ++ " " ++ f y0 ++ String.concat (List.map2 segment points rest)

        [] ->
            ""


segment : ( Float, Float ) -> ( Float, Float ) -> String
segment ( x0, y0 ) ( x1, y1 ) =
    let
        ym =
            f ((y0 + y1) / 2)
    in
    " C" ++ f x0 ++ " " ++ ym ++ " " ++ f x1 ++ " " ++ ym ++ " " ++ f x1 ++ " " ++ f y1


nearSource : List ( Float, Float ) -> Maybe ( Float, Float )
nearSource points =
    case points of
        p0 :: p1 :: _ ->
            Just (bezierAt 0.3 p0 p1)

        _ ->
            Nothing


nearTarget : List ( Float, Float ) -> Maybe ( Float, Float )
nearTarget points =
    case List.reverse points of
        p1 :: p0 :: _ ->
            Just (bezierAt 0.7 p0 p1)

        _ ->
            Nothing


midpoint : List ( Float, Float ) -> Maybe ( Float, Float )
midpoint points =
    case List.drop ((List.length points - 2) // 2) points of
        p0 :: p1 :: _ ->
            Just (bezierAt 0.5 p0 p1)

        _ ->
            Nothing


{-| `segment` と同じ制御点を持つ3次ベジェ上の点。
-}
bezierAt : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
bezierAt t ( x0, y0 ) ( x1, y1 ) =
    let
        ym =
            (y0 + y1) / 2

        u =
            1 - t

        b0 =
            u * u * u

        b1 =
            3 * u * u * t

        b2 =
            3 * u * t * t

        b3 =
            t * t * t
    in
    ( (b0 + b1) * x0 + (b2 + b3) * x1
    , b0 * y0 + (b1 + b2) * ym + b3 * y1
    )


viewNode : Set Int -> Prepared -> Node -> Svg Msg
viewNode done p node =
    case ( Dict.get node.id p.layout.nodes, Dict.get node.id p.contents ) of
        ( Just placed, Just content ) ->
            let
                isDone =
                    Set.member node.id done

                color =
                    phaseColor p.phases node.annots.phase

                ink =
                    if isDone then
                        inkDim

                    else
                        inkPrimary

                ink2 =
                    if isDone then
                        inkDim

                    else
                        inkSecondary

                rect =
                    Svg.rect
                        [ SA.x "0"
                        , SA.y "0"
                        , SA.width (f placed.w)
                        , SA.height (f placed.h)
                        , SA.rx "10"
                        , SA.fill
                            (if isDone then
                                "#f0efec"

                             else
                                "#ffffff"
                            )
                        , SA.stroke
                            (if isDone then
                                strokeDim

                             else
                                color
                            )
                        , SA.strokeWidth "1.5"
                        ]
                        []

                textLine size weight fill lh line ( y, acc ) =
                    ( y + lh
                    , Svg.text_
                        [ SA.x (f padX)
                        , SA.y (f (y + size))
                        , SA.fontSize (f size)
                        , SA.fontWeight weight
                        , SA.fill fill
                        ]
                        [ Svg.text line ]
                        :: acc
                    )

                sectionLines size weight fill lh lines ( y, acc ) =
                    if List.isEmpty lines then
                        ( y, acc )

                    else
                        List.foldl (textLine size weight fill lh) ( y + sectionGap, acc ) lines

                ( _, textsRev ) =
                    List.foldl (textLine labelSize "600" ink labelLH) ( padY, [] ) content.label
                        |> sectionLines smallSize "normal" ink2 smallLH content.items
                        |> sectionLines smallSize "normal" ink smallLH content.badges
                        |> sectionLines smallSize "normal" ink2 smallLH content.detail

                badge =
                    if isDone then
                        [ Svg.circle [ SA.cx "0", SA.cy "0", SA.r "9", SA.fill "#0ca30c" ] []
                        , Svg.text_
                            [ SA.x "0"
                            , SA.y "4"
                            , SA.textAnchor "middle"
                            , SA.fontSize "12"
                            , SA.fontWeight "bold"
                            , SA.fill "#ffffff"
                            ]
                            [ Svg.text "✓" ]
                        ]

                    else
                        [ Svg.circle [ SA.cx "0", SA.cy "0", SA.r "5", SA.fill color ] [] ]
            in
            Svg.g
                [ SE.onClick (ToggleDone node.id)
                , SA.css [ Css.cursor Css.pointer ]
                , SA.transform ("translate(" ++ f placed.x ++ "," ++ f placed.y ++ ")")
                ]
                (rect
                    :: List.reverse textsRev
                    ++ badge
                    ++ [ Svg.title []
                            [ Svg.text
                                (if isDone then
                                    "完了 (クリックで戻す)"

                                 else
                                    "クリックで完了にする"
                                )
                            ]
                       ]
                )

        _ ->
            Svg.g [] []


viewFinal : Maybe Layout.Final -> List (Svg Msg)
viewFinal final =
    case final of
        Nothing ->
            []

        Just fin ->
            let
                ( x, y ) =
                    fin.center
            in
            [ Svg.path
                [ SA.d (pathD fin.path)
                , SA.fill "none"
                , SA.stroke inkSecondary
                , SA.strokeWidth "1.5"
                , SA.markerEnd "url(#arrow)"
                ]
                []
            , Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "10", SA.fill "#ffffff", SA.stroke inkPrimary, SA.strokeWidth "1.5" ] []
            , Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "6", SA.fill inkPrimary ] []
            ]


f : Float -> String
f =
    String.fromFloat
