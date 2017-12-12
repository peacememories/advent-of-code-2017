module Challenge01 exposing (captcha, readCaptcha)

import Result.Extra as RExtra


captcha : List Int -> Int
captcha list =
    let
        nextItemList =
            case ( List.head list, List.tail list ) of
                ( Just x, Just xs ) ->
                    xs ++ [ x ]

                ( Just x, Nothing ) ->
                    [ x ]

                _ ->
                    []

        pairList =
            List.map2 (,) list nextItemList

        filter ( a, b ) =
            a == b
    in
        List.filter filter pairList
            |> List.map Tuple.first
            |> List.sum


toList : String -> Result String (List Int)
toList str =
    String.split "" str
        |> List.map String.toInt
        |> RExtra.combine


readCaptcha : String -> Result String Int
readCaptcha str =
    toList str
        |> Result.map captcha
