module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy)
import List as L
import List.Extra exposing (dropWhile, takeWhile, transpose)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { l : Float
    , h : Float
    }


type alias OutputModel =
    List (List Float)


model : Model
model =
    { l = 1
    , h = 999
    }



-- UPDATE


type Msg
    = ChangeMin String
    | ChangeMax String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMin v ->
            { model | l = parseOr model.l v }

        ChangeMax v ->
            { model | h = parseOr model.h v }


parseOr : Float -> String -> Float
parseOr def =
    Result.withDefault def << String.toFloat


speeds : List Float
speeds =
    L.map ((*) 10 << toFloat) <| L.range 10 20


high : List Float
high =
    L.map ((*) 0.25 << toFloat) (L.range 1 16)
        ++ L.map ((*) 0.5 << toFloat) (L.range 9 16)


solve : Model -> OutputModel
solve model =
    L.map
        (\s ->
            takeWhile (\v -> v * s <= model.h) <|
                dropWhile (\v -> v * s < model.l) high
        )
        speeds



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "DDR High Speed" ]
        , inputNum "1" ChangeMin
        , inputNum "999" ChangeMax
        , lazy viewSolve <| solve model
        ]


inputNum : String -> (String -> Msg) -> Html Msg
inputNum num msg =
    input
        [ Attr.type_ "number"
        , Attr.min "1"
        , Attr.max "999"
        , Attr.placeholder num
        , onInput msg
        ]
        []


viewSolve : OutputModel -> Html Msg
viewSolve outputModel =
    div []
        [ table []
            (tr [] (L.map (toString >> (\s -> th [] [ text s ])) speeds)
                :: L.map
                    (\row ->
                        tr []
                            (L.map
                                (\col -> td [] [ toString col |> text ])
                                row
                            )
                    )
                    (transpose outputModel)
            )
        ]
