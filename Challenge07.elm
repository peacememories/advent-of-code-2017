module Challenge07 exposing (..)

import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Style exposing (..)
import Char

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
            ]



-- Challenge Part 1 --
-- Util --
-- parseInput : String -> Result String (List (String, Int, List String))
-- inputParser : Parser state (List (String, Int, List String))
-- inputParser =
--     let
--         edgeList = sepBy ()
--         lineParser = sepBy newline ()


edgeList : Parser (List String)
edgeList =
    let
        separator =
            symbol ","

        name =
            keep oneOrMore (\c -> Char.)
    in
    surroundedBy whitespace (sepBy symbol)


sepBy : Parser a -> Parser b -> Parser (List b)
sepBy separator parser =
    let
        part =
            succeed identity
                |. separator
                |= parser
    in
    succeed (::)
        |= parser
        |= repeat zeroOrMore part


surroundedBy : Parser a -> Parser b -> Parser b
surroundedBy wrapper parser =
    succeed identity
        |. wrapper
        |= parser
        |. wrapper
