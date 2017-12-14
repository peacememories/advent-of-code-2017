module Challenge05 exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input
import Style exposing (..)
import Result.Extra as RExtra
import Array exposing (Array)


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
                case parseInput model.input of
                    Result.Ok result ->
                        toString (challenge05a result)

                    Result.Err err ->
                        err
            , text <|
                case parseInput model.input of
                    Result.Ok result ->
                        toString (challenge05b result)

                    Result.Err err ->
                        err
            ]



-- Challenge Part 1 --


challenge05a : List Int -> Int
challenge05a input =
    fixPoint (jump (\a -> a + 1)) ( 0, 0, Array.fromList input )
        |> (\( step, _, _ ) -> step)


jump : (Int -> Int) -> ( Int, Int, Array Int ) -> Maybe ( Int, Int, Array Int )
jump modifier ( step, pos, space ) =
    let
        value =
            Array.get pos space

        newArray =
            updateArray pos modifier space

        newPos =
            Maybe.map (\a -> pos + a) value
    in
        Maybe.map (\newPos -> ( step + 1, newPos, newArray )) newPos



-- Challenge Part 2 --


challenge05b : List Int -> Int
challenge05b input =
    let
        modifier a =
            if a >= 3 then
                a - 1
            else
                a + 1
    in
        fixPoint (jump modifier) ( 0, 0, Array.fromList input )
            |> (\( step, _, _ ) -> step)



-- Util --


parseInput : String -> Result String (List Int)
parseInput input =
    String.lines input
        |> List.map String.toInt
        |> RExtra.combine


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray pos f array =
    case Array.get pos array of
        Just a ->
            Array.set pos (f a) array

        Nothing ->
            array


fixPoint : (a -> Maybe a) -> a -> a
fixPoint f begin =
    case f begin of
        Just result ->
            fixPoint f result

        Nothing ->
            begin
