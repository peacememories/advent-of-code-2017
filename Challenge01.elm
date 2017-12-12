module Challenge01 exposing (captcha1, captcha2)

import Result.Extra as RExtra


captcha1 : String -> Result String Int
captcha1 str =
    toList str
        |> Result.map simpleCaptcha


captcha2 : String -> Result String Int
captcha2 str =
    toList str
        |> Result.map halfCaptcha


simpleCaptcha : List Int -> Int
simpleCaptcha list =
    let
        pairList =
            List.map2 (,) list (rotateList 1 list)

        filter ( a, b ) =
            a == b
    in
        List.filter filter pairList
            |> List.map Tuple.first
            |> List.sum


halfCaptcha : List Int -> Int
halfCaptcha list =
    let
        len =
            List.length list

        pairList =
            List.map2 (,) list (rotateList (len // 2) list)

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


rotateList : Int -> List a -> List a
rotateList amount list =
    case list of
        [] ->
            []

        list ->
            let
                len =
                    List.length list

                repeatedList =
                    List.repeat (amount // len + 1) list
                        |> List.concat
            in
                (List.drop amount repeatedList)
                    ++ (List.take amount repeatedList)
                    |> List.take len
