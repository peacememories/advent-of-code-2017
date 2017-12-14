module Challenge04 exposing (main)

import List.Extra as LExtra
import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input
import Style exposing (..)
import String


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- Model --


type Msg
    = InputUpdated String


type alias Model =
    { input : String }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputUpdated str ->
            { model
                | input = str
            }


init : Model
init =
    { input = "" }



-- View --


type Style
    = None


styles : StyleSheet Style variation
styles =
    styleSheet []


view : Model -> Html Msg
view model =
    layout styles <|
        column None
            []
            [ Input.multiline None
                []
                { onChange = InputUpdated
                , value = model.input
                , label = Input.labelAbove <| text "Input"
                , options = []
                }
            , text <|
                (challenge04a model.input
                    |> toString
                )
            , text <|
                (challenge04b model.input
                    |> toString
                )
            ]



-- Challenge Part 1 --


challenge04a : String -> Int
challenge04a pwds =
    let
        isValid password =
            String.words password
                |> LExtra.allDifferent
    in
        checkValidity isValid pwds



-- Challenge Part 2 --


challenge04b : String -> Int
challenge04b pwds =
    let
        isValid password =
            String.words password
                |> List.map deAnagram
                |> LExtra.allDifferent
    in
        checkValidity isValid pwds


deAnagram : String -> String
deAnagram str =
    String.split "" str
        |> List.sort
        |> String.concat



-- Util --


checkValidity : (String -> Bool) -> String -> Int
checkValidity checker input =
    String.lines input
        |> List.map checker
        |> List.map
            (\x ->
                if x then
                    1
                else
                    0
            )
        |> List.sum
