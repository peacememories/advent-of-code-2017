module Challenge06 exposing (main)

import Array exposing (Array)
import Array.Extra
import Element exposing (..)
import Element.Input as Input
import EverySet
import Html exposing (Html)
import Lazy.List exposing (LazyList)
import Lazy.List.Extra
import List.Extra
import Result.Extra
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
                case parseInput model.input of
                    Result.Ok result ->
                        toString (challenge06a result)

                    Result.Err err ->
                        err
            , text <|
                case parseInput model.input of
                    Result.Ok result ->
                        toString (challenge06b result)

                    Result.Err err ->
                        err
            ]



-- Challenge Part 1 --


challenge06a : Array Int -> Int
challenge06a memory =
    Lazy.List.Extra.compute rebalance memory
        |> takeWhileUnique
        |> Lazy.List.length


rebalance : Array Int -> Array Int
rebalance memory =
    case maxArray memory of
        Just ( index, value ) ->
            let
                newBase =
                    Array.set index 0 memory

                additions =
                    List.repeat (index + 1) 0 ++ List.repeat value 1
            in
            foldingAdd (Array.length memory) additions
                |> Array.fromList
                |> Array.Extra.map2 (+) newBase

        Nothing ->
            memory



-- Challenge Part 2 --


challenge06b : Array Int -> Int
challenge06b memory =
    let
        rebalanceList =
            Lazy.List.Extra.compute rebalance memory

        uniqueList =
            takeWhileUnique rebalanceList

        firstRepetition =
            Lazy.List.drop (Lazy.List.length uniqueList) rebalanceList
                |> Lazy.List.head
    in
    Maybe.map
        (\firstRepetition ->
            Lazy.List.dropWhile (\elem -> elem /= firstRepetition) uniqueList
                |> Lazy.List.length
        )
        firstRepetition
        |> Maybe.withDefault 0



-- Util --


maxArray : Array comparable -> Maybe ( Int, comparable )
maxArray array =
    let
        folder ( index, value ) state =
            case state of
                Just ( maxIndex, maxValue ) ->
                    if value > maxValue then
                        Just ( index, value )
                    else
                        state

                Nothing ->
                    Just ( index, value )
    in
    Array.toIndexedList array
        |> List.foldl folder Nothing


foldingAdd : Int -> List number -> List number
foldingAdd length list =
    List.Extra.greedyGroupsOf length list
        |> List.foldl
            (\left right ->
                greedyMap2 (+) left right
            )
            (List.repeat length 0)


greedyMap2 : (a -> a -> a) -> List a -> List a -> List a
greedyMap2 f left right =
    case ( left, right ) of
        ( x :: xs, y :: ys ) ->
            f x y :: greedyMap2 f xs ys

        ( x :: xs, [] ) ->
            x :: greedyMap2 f xs []

        ( [], y :: ys ) ->
            y :: greedyMap2 f [] ys

        ( [], [] ) ->
            []


takeWhileUnique : LazyList a -> LazyList a
takeWhileUnique list =
    let
        foldFunction element uniques =
            if EverySet.member element uniques then
                ( Nothing, uniques )
            else
                ( Just element, EverySet.insert element uniques )

        isJust maybe =
            case maybe of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Lazy.List.Extra.scanState foldFunction EverySet.empty list
        |> Lazy.List.takeWhile isJust
        |> Lazy.List.filterMap identity


parseInput : String -> Result String (Array Int)
parseInput input =
    String.split "\t" input
        |> List.map String.toInt
        |> Result.Extra.combine
        |> Result.map Array.fromList
