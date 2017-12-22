module Challenge07 exposing (..)

import Char
import Element exposing (..)
import Element.Input as Input
import Graph.Tree as Tree exposing (Tree)
import Html exposing (Html)
import Parser exposing (..)
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
            ]



-- Challenge Part 1 --
-- Util --
-- parseInput : String -> Result String (List (String, Int, List String))
-- inputParser : Parser state (List (String, Int, List String))
-- inputParser =
--     let
--         edgeList = sepBy ()
--         lineParser = sepBy newline ()
-- insertNode : InputLine -> Dict String (Tree String) -> List (Tree String)
-- insertNode {name, connections} forest =
--     let
--         newNode =
--             Tree.inner name (connection
--             |> List.map (\))
--         rootFilter tree =
--             case Tree.root of
--                 Just (rootLabel, rootChildren) ->
--                     if List.member rootLabel connections then
--                         False
--                     else
--                         True
--                 Nothing ->
--                     False
--         newForest =
--             List.filter rootFilter forest
--     in


setLabel : label -> List (Tree label) -> Tree label -> Tree label
setLabel label children tree =
    case Tree.root tree of
        Just ( rootLabel, children ) ->
            if rootLabel == label then
                Tree.inner label children
            else
                Tree.inner rootLabel
                    (children
                        |> List.map (setLabel label children)
                    )

        Nothing ->
            Tree.empty


findLabel : label -> Tree label -> Maybe (Tree label)
findLabel label tree =
    case Tree.root tree of
        Just ( rootLabel, children ) ->
            if rootLabel == label then
                Just tree
            else
                children
                    |> List.filterMap (findLabel label)
                    |> List.head

        Nothing ->
            Nothing



-- (label, List (Tree label)) -> Tree label
-- (label, children) =
-- Tree.inner label (List.filterMap (modifyTree modifier))


type alias InputLine =
    { name : String
    , weight : Int
    , connections : List String
    }


input : Parser (List InputLine)
input =
    sepBy (symbol "\n") line


line : Parser InputLine
line =
    let
        arrow =
            symbol "->"
    in
    succeed InputLine
        |= name
        |. whitespace
        |= weight
        |. whitespace
        |. arrow
        |. whitespace
        |= edgeList


whitespace : Parser String
whitespace =
    keep zeroOrMore (\c -> c == ' ')


name : Parser String
name =
    let
        isValidChar c =
            Char.isUpper c || Char.isLower c || Char.isDigit c
    in
    keep oneOrMore isValidChar


weight : Parser Int
weight =
    succeed identity
        |. symbol "("
        |= int
        |. symbol ")"


edgeList : Parser (List String)
edgeList =
    let
        separator =
            symbol ","
    in
    surroundedBy whitespace (sepBy (surroundedBy whitespace separator) name)


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
