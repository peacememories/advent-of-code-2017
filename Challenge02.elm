module Challenge02 exposing (main)

import Result.Extra as RExtra
import List.Extra as LExtra
import Html exposing (Html)
import Element exposing (..)
import Element.Input as Input
import Style exposing (..)


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
                case
                    parseInput model.input
                        |> Result.map checksum
                of
                    Result.Ok result ->
                        toString result

                    Result.Err err ->
                        err
            , text <|
                case
                    parseInput model.input
                        |> Result.andThen division
                of
                    Result.Ok result ->
                        toString result

                    Result.Err err ->
                        err
            ]



-- Util --


parseInput : String -> Result String (List (List Int))
parseInput str =
    String.lines str
        |> List.map parseLine
        |> RExtra.combine


parseLine : String -> Result String (List Int)
parseLine str =
    String.split "\t" str
        |> List.map String.toInt
        |> RExtra.combine



-- Challenge Part 1 --


checksum : List (List Int) -> Int
checksum list =
    List.filterMap lineChecksum list
        |> List.sum


lineChecksum : List Int -> Maybe Int
lineChecksum list =
    let
        minimum =
            List.minimum list

        maximum =
            List.maximum list
    in
        Maybe.map2 (-) maximum minimum



-- Challenge Part 2 --


division : List (List Int) -> Result String Int
division list =
    List.map lineDivision list
        |> RExtra.combine
        |> Result.map List.sum


lineDivision : List Int -> Result String Int
lineDivision list =
    LExtra.select list
        |> List.filterMap (\( x, xs ) -> findDivision x xs)
        |> List.head
        |> Result.fromMaybe
            ("No dividable members found in "
                ++ (toString list)
            )


findDivision : Int -> List Int -> Maybe Int
findDivision value list =
    let
        test a =
            if rem value a == 0 then
                Just (value // a)
            else
                Nothing
    in
        List.filterMap test list
            |> List.head
