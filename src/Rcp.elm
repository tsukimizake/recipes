module Rcp exposing
    ( Annots
    , Edge
    , Graph
    , Ing
    , Item(..)
    , Node
    , Recipe
    , buildGraph
    , fromDatums
    , metaGet
    )

import Dict exposing (Dict)
import Set exposing (Set)
import Sexp exposing (Datum(..))


type alias Recipe =
    { meta : List ( String, String )
    , notes : List String
    , bindings : List Binding
    , body : List Expr
    }


type Binding
    = Single String Expr
    | Multi (List String) Expr


type Expr
    = IngE Ing
    | ToolE String
    | OpE OpForm
    | SplitE OpForm
    | RefE String


type alias OpForm =
    { label : String
    , inputs : List Expr
    , annots : Annots
    }


type alias Ing =
    { name : String
    , qty : Maybe String
    , unit : Maybe String
    , note : Maybe String
    }


type alias Annots =
    { time : Maybe String
    , attention : Maybe String
    , heat : Maybe String
    , until : Maybe String
    , tool : Maybe String
    , note : Maybe String
    , phase : Maybe String
    }


emptyAnnots : Annots
emptyAnnots =
    Annots Nothing Nothing Nothing Nothing Nothing Nothing Nothing


metaGet : String -> Recipe -> Maybe String
metaGet key recipe =
    recipe.meta
        |> List.filter (\( k, _ ) -> k == key)
        |> List.head
        |> Maybe.map Tuple.second



-- Datum -> Recipe


fromDatums : List Datum -> Result String Recipe
fromDatums datums =
    case datums of
        [ L ((Sym "recipe") :: parts) ] ->
            List.foldl recipePart (Ok emptyRecipe) parts

        _ ->
            Err "トップレベルは (recipe ...) がちょうど1つ必要"


emptyRecipe : Recipe
emptyRecipe =
    { meta = [], notes = [], bindings = [], body = [] }


recipePart : Datum -> Result String Recipe -> Result String Recipe
recipePart part acc =
    acc
        |> Result.andThen
            (\r ->
                case part of
                    L ((Sym "meta") :: entries) ->
                        collect metaEntry entries
                            |> Result.map (\m -> { r | meta = r.meta ++ m })

                    L [ Sym "note", Str s ] ->
                        Ok { r | notes = r.notes ++ [ s ] }

                    L ((Sym "let") :: (L bindingDatums) :: bodyDatums) ->
                        Result.map2
                            (\bs body -> { r | bindings = r.bindings ++ bs, body = r.body ++ body })
                            (collect binding bindingDatums)
                            (collect expr bodyDatums)

                    _ ->
                        Err ("recipe 直下に不明な形式: " ++ Sexp.toString part)
            )


collect : (a -> Result String b) -> List a -> Result String (List b)
collect f xs =
    List.foldr (\x acc -> Result.map2 (::) (f x) acc) (Ok []) xs


metaEntry : Datum -> Result String ( String, String )
metaEntry d =
    case d of
        L [ Sym k, v ] ->
            Ok ( k, Sexp.toString v )

        _ ->
            Err ("meta の項目が不正: " ++ Sexp.toString d)


binding : Datum -> Result String Binding
binding d =
    case d of
        L [ Sym name, e ] ->
            Result.map (Single name) (expr e)

        L [ L names, e ] ->
            Result.map2 Multi (collect symName names) (expr e)

        _ ->
            Err ("束縛が不正: " ++ Sexp.toString d)


symName : Datum -> Result String String
symName d =
    case d of
        Sym s ->
            Ok s

        _ ->
            Err ("シンボルが必要: " ++ Sexp.toString d)


strValue : Datum -> Result String String
strValue d =
    case d of
        Str s ->
            Ok s

        _ ->
            Err ("文字列が必要: " ++ Sexp.toString d)


expr : Datum -> Result String Expr
expr d =
    case d of
        Sym name ->
            Ok (RefE name)

        L ((Sym "ing") :: (Sym name) :: rest) ->
            Result.map IngE (ing name rest)

        L [ Sym "tool", Sym name ] ->
            Ok (ToolE name)

        L ((Sym "op") :: (Str label) :: rest) ->
            Result.map OpE (opForm label rest)

        L ((Sym "split") :: (Str label) :: rest) ->
            Result.map SplitE (opForm label rest)

        L ((Sym "->") :: first :: forms) ->
            expr first
                |> Result.andThen (\f -> List.foldl threadStep (Ok f) forms)

        _ ->
            Err ("式が不正: " ++ Sexp.toString d)


threadStep : Datum -> Result String Expr -> Result String Expr
threadStep formD acc =
    acc
        |> Result.andThen
            (\prev ->
                expr formD
                    |> Result.andThen
                        (\form ->
                            case form of
                                OpE f ->
                                    Ok (OpE { f | inputs = prev :: f.inputs })

                                SplitE f ->
                                    Ok (SplitE { f | inputs = prev :: f.inputs })

                                _ ->
                                    Err ("-> の中は op か split のみ: " ++ Sexp.toString formD)
                        )
            )


opForm : String -> List Datum -> Result String OpForm
opForm label rest =
    let
        ( positional, annotDatums ) =
            span (not << isKw) rest
    in
    Result.map2 (OpForm label)
        (collect expr positional)
        (annotations annotDatums)


isKw : Datum -> Bool
isKw d =
    case d of
        Kw _ ->
            True

        _ ->
            False


span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    case xs of
        x :: rest ->
            if p x then
                let
                    ( a, b ) =
                        span p rest
                in
                ( x :: a, b )

            else
                ( [], xs )

        [] ->
            ( [], [] )


annotations : List Datum -> Result String Annots
annotations ds =
    case ds of
        [] ->
            Ok emptyAnnots

        (Kw k) :: v :: rest ->
            annotations rest
                |> Result.andThen (setAnnot k v)

        [ Kw k ] ->
            Err (":" ++ k ++ " の値がない")

        other :: _ ->
            Err ("注釈の後に位置引数がある: " ++ Sexp.toString other)


setAnnot : String -> Datum -> Annots -> Result String Annots
setAnnot k v a =
    case k of
        "time" ->
            Result.map (\t -> { a | time = Just t }) (timeValue v)

        "attention" ->
            case v of
                Kw s ->
                    Ok { a | attention = Just s }

                _ ->
                    Err (":attention の値が不正: " ++ Sexp.toString v)

        "heat" ->
            case v of
                Sym s ->
                    Ok { a | heat = Just s }

                Kw "on" ->
                    Ok { a | heat = Just "on" }

                _ ->
                    Err (":heat の値が不正: " ++ Sexp.toString v)

        "until" ->
            Result.map (\s -> { a | until = Just s }) (strValue v)

        "tool" ->
            Result.map (\s -> { a | tool = Just s }) (symName v)

        "note" ->
            Result.map (\s -> { a | note = Just s }) (strValue v)

        "phase" ->
            Result.map (\s -> { a | phase = Just s }) (symName v)

        _ ->
            Ok a


timeValue : Datum -> Result String String
timeValue v =
    case v of
        L [ Num n, Sym u ] ->
            Ok (Sexp.fmtNum n ++ " " ++ u)

        L [ Num lo, Num hi, Sym u ] ->
            Ok (Sexp.fmtNum lo ++ "〜" ++ Sexp.fmtNum hi ++ " " ++ u)

        _ ->
            Err (":time の値が不正: " ++ Sexp.toString v)


ing : String -> List Datum -> Result String Ing
ing name rest =
    case rest of
        [] ->
            Ok (Ing name Nothing Nothing Nothing)

        [ Str note ] ->
            Ok (Ing name Nothing Nothing (Just note))

        [ q, Sym unit ] ->
            Result.map (\qs -> Ing name (Just qs) (Just unit) Nothing) (qtyValue q)

        [ q, Sym unit, Str note ] ->
            Result.map (\qs -> Ing name (Just qs) (Just unit) (Just note)) (qtyValue q)

        _ ->
            Err ("ing が不正: " ++ name)


qtyValue : Datum -> Result String String
qtyValue q =
    case q of
        Num n ->
            Ok (Sexp.fmtNum n)

        Str s ->
            Ok s

        L [ Num lo, Num hi ] ->
            Ok (Sexp.fmtNum lo ++ "〜" ++ Sexp.fmtNum hi)

        _ ->
            Err ("数量が不正: " ++ Sexp.toString q)



-- Recipe -> Graph


type alias Node =
    { id : Int
    , label : String
    , items : List Item
    , annots : Annots
    }


type Item
    = IngItem Ing
    | ToolItem String


type alias Edge =
    { from : Int
    , to : Int
    , label : Maybe String
    }


type alias Graph =
    { nodes : List Node
    , edges : List Edge
    , result : Maybe Int
    }


type Value
    = VNode Int
    | VItem Item


type BindingDef
    = SingleDef Expr
    | MultiDef (List String) Expr


type alias Builder =
    { nextId : Int
    , nodes : List Node
    , edges : List Edge
    , memo : Dict String Value
    , inProgress : Set String
    , defs : Dict String BindingDef
    }


{-| 全束縛を定義順に評価してから本体を評価する。未消費の束縛もノードとして現れる。
-}
buildGraph : Recipe -> Result String Graph
buildGraph recipe =
    let
        defs =
            recipe.bindings
                |> List.concatMap
                    (\b ->
                        case b of
                            Single n e ->
                                [ ( n, SingleDef e ) ]

                            Multi ns e ->
                                List.map (\n -> ( n, MultiDef ns e )) ns
                    )

        b0 =
            { nextId = 0
            , nodes = []
            , edges = []
            , memo = Dict.empty
            , inProgress = Set.empty
            , defs = Dict.fromList defs
            }
    in
    List.foldl
        (\( n, _ ) acc -> Result.andThen (\b -> Result.map Tuple.second (resolveRef n b)) acc)
        (Ok b0)
        defs
        |> Result.andThen (evalBody recipe.body)
        |> Result.map
            (\( result, b ) ->
                { nodes = List.reverse b.nodes
                , edges = List.reverse b.edges
                , result = result
                }
            )


evalBody : List Expr -> Builder -> Result String ( Maybe Int, Builder )
evalBody exprs b0 =
    List.foldl
        (\e acc ->
            acc
                |> Result.andThen
                    (\( _, b ) ->
                        evalExpr e b
                            |> Result.map (\( v, b2 ) -> ( valueNode v, b2 ))
                    )
        )
        (Ok ( Nothing, b0 ))
        exprs


valueNode : Value -> Maybe Int
valueNode v =
    case v of
        VNode i ->
            Just i

        VItem _ ->
            Nothing


resolveRef : String -> Builder -> Result String ( Value, Builder )
resolveRef name b =
    case Dict.get name b.memo of
        Just v ->
            Ok ( v, b )

        Nothing ->
            if Set.member name b.inProgress then
                Err ("束縛が循環している: " ++ name)

            else
                case Dict.get name b.defs of
                    Nothing ->
                        Err ("未定義の束縛: " ++ name)

                    Just (SingleDef e) ->
                        evalExpr e { b | inProgress = Set.insert name b.inProgress }
                            |> Result.map
                                (\( v, b2 ) ->
                                    ( v
                                    , { b2
                                        | memo = Dict.insert name v b2.memo
                                        , inProgress = Set.remove name b2.inProgress
                                      }
                                    )
                                )

                    Just (MultiDef names e) ->
                        evalExpr e { b | inProgress = Set.union (Set.fromList names) b.inProgress }
                            |> Result.andThen
                                (\( v, b2 ) ->
                                    case v of
                                        VNode _ ->
                                            Ok
                                                ( v
                                                , { b2
                                                    | memo = List.foldl (\n m -> Dict.insert n v m) b2.memo names
                                                    , inProgress = Set.diff b2.inProgress (Set.fromList names)
                                                  }
                                                )

                                        VItem _ ->
                                            Err ("分配束縛の右辺が操作ではない: " ++ String.join " " names)
                                )


evalExpr : Expr -> Builder -> Result String ( Value, Builder )
evalExpr e b =
    case e of
        IngE i ->
            Ok ( VItem (IngItem i), b )

        ToolE t ->
            Ok ( VItem (ToolItem t), b )

        RefE name ->
            resolveRef name b

        OpE form ->
            buildNode form b

        SplitE form ->
            buildNode form b


buildNode : OpForm -> Builder -> Result String ( Value, Builder )
buildNode form b0 =
    let
        step input acc =
            acc
                |> Result.andThen
                    (\( items, incoming, b ) ->
                        evalExpr input b
                            |> Result.map
                                (\( v, b2 ) ->
                                    case v of
                                        VItem item ->
                                            ( item :: items, incoming, b2 )

                                        VNode from ->
                                            ( items, ( from, refName input ) :: incoming, b2 )
                                )
                    )
    in
    List.foldl step (Ok ( [], [], b0 )) form.inputs
        |> Result.map
            (\( items, incoming, b ) ->
                let
                    id =
                        b.nextId

                    node =
                        { id = id
                        , label = form.label
                        , items = List.reverse items
                        , annots = form.annots
                        }
                in
                ( VNode id
                , { b
                    | nextId = id + 1
                    , nodes = node :: b.nodes
                    , edges = List.map (\( from, lbl ) -> Edge from id lbl) incoming ++ b.edges
                  }
                )
            )


refName : Expr -> Maybe String
refName e =
    case e of
        RefE n ->
            Just n

        _ ->
            Nothing
