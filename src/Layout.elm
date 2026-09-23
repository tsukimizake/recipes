module Layout exposing (Config, Final, Layout, Placed, PlacedEdge, compute)

import Dict exposing (Dict)
import Rcp exposing (Edge)
import Set


type alias Config =
    { nodeW : Float
    , gapX : Float
    , gapY : Float
    , margin : Float
    , dummyW : Float
    , finalR : Float
    }


type alias Placed =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias PlacedEdge =
    { from : Int
    , to : Int
    , label : Maybe String
    , points : List ( Float, Float )
    }


type alias Final =
    { path : List ( Float, Float )
    , center : ( Float, Float )
    }


type alias Layout =
    { nodes : Dict Int Placed
    , edges : List PlacedEdge
    , width : Float
    , height : Float
    , final : Maybe Final
    }


type alias Chain =
    { edge : Edge
    , nodes : List Int
    }


{-| 層割り当ては ASAP (依存が解けた直後の層)。長い辺は負のIDのダミーノードで分割し、
層内の並び順はバリセンタ法で決める。1単位 = nodeW + gapX。
-}
compute : Config -> List Int -> Dict Int Float -> List Edge -> Maybe Int -> Layout
compute cfg ids heights edges result =
    let
        idSet =
            Set.fromList ids

        validEdges =
            List.filter (\e -> Set.member e.from idSet && Set.member e.to idSet && e.from /= e.to) edges

        pairs =
            List.map (\e -> ( e.from, e.to )) validEdges

        succs =
            adjacency pairs

        preds =
            adjacency (List.map swap pairs)

        order =
            topoOrder ids succs preds

        asap =
            List.foldl (\n d -> Dict.insert n (1 + maxOf (get n preds) d) d) Dict.empty order

        toSink =
            List.foldl (\n d -> Dict.insert n (1 + maxOf (get n succs) d) d) Dict.empty (List.reverse order)

        hMax =
            Dict.values asap |> List.maximum |> Maybe.withDefault 0

        layerOfReal n =
            Dict.get n asap |> Maybe.withDefault 0

        ( chains, dummyInfo ) =
            buildChains layerOfReal validEdges

        dummyIds =
            Dict.keys dummyInfo

        layerOf n =
            if n < 0 then
                Dict.get n dummyInfo |> Maybe.map .layer |> Maybe.withDefault 0

            else
                layerOfReal n

        depthWeight depths n =
            1 + toFloat (Dict.get n depths |> Maybe.withDefault 0)

        weightDown n =
            if n < 0 then
                Dict.get n dummyInfo |> Maybe.map (.from >> depthWeight asap) |> Maybe.withDefault 1

            else
                depthWeight asap n

        weightUp n =
            if n < 0 then
                Dict.get n dummyInfo |> Maybe.map (.to >> depthWeight toSink) |> Maybe.withDefault 1

            else
                depthWeight toSink n

        allIds =
            ids ++ dummyIds

        layers =
            List.range 0 hMax
                |> List.map (\i -> List.filter (\n -> layerOf n == i) allIds)

        wOf n =
            if n < 0 then
                cfg.dummyW

            else
                1

        chainPairs =
            List.concatMap (\c -> List.map2 Tuple.pair c.nodes (List.drop 1 c.nodes)) chains

        lSuccs =
            adjacency chainPairs

        lPreds =
            adjacency (List.map swap chainPairs)

        initialXs =
            layers
                |> List.concatMap (List.indexedMap (\i n -> ( n, toFloat i )))
                |> Dict.fromList

        down positions =
            List.foldl (placeLayer wOf weightDown lPreds) positions layers

        up positions =
            List.foldl (placeLayer wOf weightUp lSuccs) positions (List.reverse layers)

        xs =
            initialXs |> down |> up |> down |> up |> down

        xOf n =
            Dict.get n xs |> Maybe.withDefault 0

        unit =
            cfg.nodeW + cfg.gapX

        minLeft =
            allIds |> List.map (\n -> xOf n - wOf n / 2) |> List.minimum |> Maybe.withDefault 0

        maxRight =
            allIds |> List.map (\n -> xOf n + wOf n / 2) |> List.maximum |> Maybe.withDefault 1

        cx n =
            cfg.margin + (xOf n - minLeft) * unit

        layerH i =
            List.filter (\n -> n >= 0) (Maybe.withDefault [] (nth i layers))
                |> List.filterMap (\n -> Dict.get n heights)
                |> List.maximum
                |> Maybe.withDefault 40

        yTops =
            List.foldl
                (\i ( acc, y ) -> ( Dict.insert i y acc, y + layerH i + cfg.gapY ))
                ( Dict.empty, cfg.margin )
                (List.range 0 (hMax + 1))
                |> Tuple.first

        yTop i =
            Dict.get i yTops |> Maybe.withDefault cfg.margin

        placed =
            ids
                |> List.map
                    (\n ->
                        let
                            l =
                                layerOfReal n

                            h =
                                Dict.get n heights |> Maybe.withDefault 40
                        in
                        ( n
                        , { x = cx n - cfg.nodeW / 2
                          , y = yTop l + (layerH l - h) / 2
                          , w = cfg.nodeW
                          , h = h
                          }
                        )
                    )
                |> Dict.fromList

        bottomOf n =
            Dict.get n placed
                |> Maybe.map (\p -> ( p.x + p.w / 2, p.y + p.h ))
                |> Maybe.withDefault ( cx n, yTop (layerOf n) )

        topOf n =
            Dict.get n placed
                |> Maybe.map (\p -> ( p.x + p.w / 2, p.y ))
                |> Maybe.withDefault ( cx n, yTop (layerOf n) )

        midOf n =
            ( cx n, yTop (layerOf n) + layerH (layerOf n) / 2 )

        chainPoints c =
            case c.nodes of
                first :: rest ->
                    bottomOf first
                        :: List.map
                            (\n ->
                                if n < 0 then
                                    midOf n

                                else
                                    topOf n
                            )
                            rest

                [] ->
                    []

        placedEdges =
            List.map
                (\c -> { from = c.edge.from, to = c.edge.to, label = c.edge.label, points = chainPoints c })
                chains

        bodyBottom =
            yTop (hMax + 1) - cfg.gapY

        final =
            result
                |> Maybe.andThen
                    (\r ->
                        if Dict.member r placed then
                            let
                                ( bx, by ) =
                                    bottomOf r

                                centerY =
                                    bodyBottom + cfg.gapY + cfg.finalR
                            in
                            Just
                                { path = [ ( bx, by ), ( bx, centerY - cfg.finalR ) ]
                                , center = ( bx, centerY )
                                }

                        else
                            Nothing
                    )

        height =
            case final of
                Just _ ->
                    bodyBottom + cfg.gapY + 2 * cfg.finalR + cfg.margin

                Nothing ->
                    bodyBottom + cfg.margin
    in
    { nodes = placed
    , edges = placedEdges
    , width = cfg.margin * 2 + (maxRight - minLeft) * unit
    , height = height
    , final = final
    }


{-| 層内の各ノードを隣接層の接続先の加重平均位置に寄せ、重なりを左から順に解消したうえで
全体を望ましい位置の平均に再センタリングする。重みはグラフ上の深さで、下準備のような
浅い葉ノードが主系列を引っ張らないようにする。
-}
placeLayer : (Int -> Float) -> (Int -> Float) -> Dict Int (List Int) -> List Int -> Dict Int Float -> Dict Int Float
placeLayer wOf weightOf nbrs layerNodes xs =
    let
        current n =
            Dict.get n xs |> Maybe.withDefault 0

        desired n =
            case List.filterMap (\m -> Dict.get m xs |> Maybe.map (Tuple.pair (weightOf m))) (get n nbrs) of
                [] ->
                    current n

                ws ->
                    List.sum (List.map (\( w, x ) -> w * x) ws) / List.sum (List.map Tuple.first ws)

        sorted =
            layerNodes
                |> List.map (\n -> ( n, desired n ))
                |> List.sortBy (\( n, d ) -> ( d, current n ))

        ( placedRev, _ ) =
            List.foldl
                (\( n, d ) ( acc, prev ) ->
                    let
                        x =
                            case prev of
                                Nothing ->
                                    d

                                Just ( px, pw ) ->
                                    max d (px + (pw + wOf n) / 2)
                    in
                    ( ( n, x ) :: acc, Just ( x, wOf n ) )
                )
                ( [], Nothing )
                sorted

        shift =
            mean (List.map Tuple.second sorted) - mean (List.map Tuple.second placedRev)
    in
    List.foldl (\( n, x ) d -> Dict.insert n (x + shift) d) xs placedRev


buildChains : (Int -> Int) -> List Edge -> ( List Chain, Dict Int { layer : Int, from : Int, to : Int } )
buildChains layerOf edges =
    let
        step e ( chains, dummyInfo, next ) =
            let
                lu =
                    layerOf e.from

                count =
                    layerOf e.to - lu - 1
            in
            if count <= 0 then
                ( { edge = e, nodes = [ e.from, e.to ] } :: chains, dummyInfo, next )

            else
                let
                    dummies =
                        List.range 0 (count - 1)
                            |> List.map (\i -> ( next - i, lu + 1 + i ))
                in
                ( { edge = e, nodes = e.from :: List.map Tuple.first dummies ++ [ e.to ] } :: chains
                , List.foldl (\( d, l ) m -> Dict.insert d { layer = l, from = e.from, to = e.to } m) dummyInfo dummies
                , next - count
                )
    in
    List.foldl step ( [], Dict.empty, -1 ) edges
        |> (\( cs, dl, _ ) -> ( List.reverse cs, dl ))


topoOrder : List Int -> Dict Int (List Int) -> Dict Int (List Int) -> List Int
topoOrder ids succs preds =
    let
        indeg =
            ids |> List.map (\n -> ( n, List.length (get n preds) )) |> Dict.fromList

        ordered =
            kahn succs indeg (List.filter (\n -> Dict.get n indeg == Just 0) ids) []

        missing =
            List.filter (\n -> not (List.member n ordered)) ids
    in
    ordered ++ missing


kahn : Dict Int (List Int) -> Dict Int Int -> List Int -> List Int -> List Int
kahn succs indeg queue acc =
    case queue of
        [] ->
            List.reverse acc

        n :: rest ->
            let
                ( indeg2, freed ) =
                    List.foldl
                        (\s ( d, z ) ->
                            let
                                v =
                                    (Dict.get s d |> Maybe.withDefault 0) - 1
                            in
                            ( Dict.insert s v d
                            , if v == 0 then
                                s :: z

                              else
                                z
                            )
                        )
                        ( indeg, [] )
                        (get n succs)
            in
            kahn succs indeg2 (rest ++ List.reverse freed) (n :: acc)


adjacency : List ( Int, Int ) -> Dict Int (List Int)
adjacency pairs =
    List.foldr
        (\( k, v ) d -> Dict.update k (\m -> Just (v :: Maybe.withDefault [] m)) d)
        Dict.empty
        pairs


get : Int -> Dict Int (List Int) -> List Int
get n d =
    Dict.get n d |> Maybe.withDefault []


maxOf : List Int -> Dict Int Int -> Int
maxOf ns d =
    ns |> List.filterMap (\n -> Dict.get n d) |> List.maximum |> Maybe.withDefault -1


mean : List Float -> Float
mean xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


nth : Int -> List a -> Maybe a
nth i xs =
    List.drop i xs |> List.head


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )
