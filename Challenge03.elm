module Challenge03 exposing (..)

import List.Extra as LExtra
import Array
import Stream exposing (Stream)


spiralDistance : Int -> Int
spiralDistance value =
    0


sideDistances : Int -> List Int
sideDistances length =
    case length of
        0 ->
            []

        n ->
            if rem n 2 == 1 then
                [ n // 2 + 1 ] ++ (sideDistances (n - 1))
            else
                (sideDistances (n - 1)) ++ [ n // 2 + 1 ]


ringDistances : Int -> List Int
ringDistances sideLength =
    LExtra.cycle (4 * sideLength) (sideDistances sideLength)


baseDistance : Int -> Int
baseDistance value =
    let
        base =
            floor <| sqrt <| toFloat (value - 1)

        size =
            base + (rem base 2) - 1
    in
        size // 2


positionDistance : Int -> Int
positionDistance value =
    let
        base =
            floor <| sqrt <| toFloat (value - 1)

        size =
            base + (rem base 2)

        smallSize =
            size - 1
    in
        ringDistances size
            |> Array.fromList
            |> Array.get (value - (smallSize ^ 2) - 1)
            |> Maybe.withDefault 0


distance : Int -> Int
distance value =
    (positionDistance value) + (baseDistance value)


type alias Pos =
    ( Int, Int )


add : Pos -> Pos -> Pos
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


moves : Stream Pos
moves =
    let
        circle n =
            List.repeat (2 * n - 1) ( 1, 0 )
                ++ List.repeat (2 * n - 1) ( 0, 1 )
                ++ List.repeat (2 * n) ( -1, 0 )
                ++ List.repeat (2 * n) ( 0, -1 )
    in
        Stream.create (\a -> Stream.continue (a + 1)) 1
            |> Stream.concatMap (circle >> Stream.fromList)



-- scanl : (a -> b -> b) -> b -> LazyList a -> LazyList b
-- scanl f start list =
--     case LazyList.headAndTail list of
--         Just ( head, tail ) ->
--             let
--                 newPos =
--                     f head start
--             in
--                 Lazy.lazy (\() -> LazyList.Cons start (scanl f newPos tail))
--         Nothing ->
--             Lazy.lazy (\() -> LazyList.Nil)
-- positions : LazyList Pos
-- positions =
--     scanl add ( 0, 0 ) moves
