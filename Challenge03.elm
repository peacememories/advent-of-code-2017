module Challenge03 exposing (challenge03a, challenge03b)

import List.Extra as LExtra
import Array
import Lazy.List as LazyList exposing (LazyList, (+++))
import Lazy
import Dict exposing (Dict)


-- Challenge Part 1 --


challenge03a : Int -> Int
challenge03a value =
    (positionDistance value) + (baseDistance value)


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



-- Challenge Part 2 --


challenge03b : Int -> Maybe Int
challenge03b value =
    findFirst (\a -> a > value) additionValues


type alias Pos =
    ( Int, Int )


add : Pos -> Pos -> Pos
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Actually not necessary because part 1 is solved differently
-}
manhattanDistance : Pos -> Int
manhattanDistance ( x, y ) =
    x + y


moves : LazyList Pos
moves =
    let
        takeOnly n v =
            LazyList.repeat v |> LazyList.take n

        circle n =
            takeOnly (2 * n - 1) ( 1, 0 )
                +++ takeOnly (2 * n - 1) ( 0, 1 )
                +++ takeOnly (2 * n) ( -1, 0 )
                +++ takeOnly (2 * n) ( 0, -1 )
    in
        LazyList.numbers
            |> LazyList.map circle
            |> LazyList.flatten


takeNth : Int -> LazyList a -> Maybe a
takeNth n list =
    case LazyList.headAndTail list of
        Just ( x, xs ) ->
            if n <= 0 then
                Just x
            else
                takeNth (n - 1) xs

        Nothing ->
            Nothing


findFirst : (a -> Bool) -> LazyList a -> Maybe a
findFirst f list =
    case LazyList.headAndTail list of
        Just ( x, xs ) ->
            if f x then
                Just x
            else
                findFirst f xs

        Nothing ->
            Nothing


scanl : (a -> b -> b) -> b -> LazyList a -> LazyList b
scanl f start list =
    case LazyList.headAndTail list of
        Just ( head, tail ) ->
            let
                newPos =
                    f head start
            in
                Lazy.lazy (\() -> LazyList.Cons start (scanl f newPos tail))

        Nothing ->
            Lazy.lazy (\() -> LazyList.Nil)


positions : LazyList Pos
positions =
    scanl add ( 0, 0 ) moves


modifyMap : Pos -> Dict Pos Int -> ( Int, Dict Pos Int )
modifyMap pos map =
    let
        modifiers =
            LazyList.fromList [ -1, 0, 1 ]

        positionModifiers =
            LazyList.product2 modifiers modifiers

        positions =
            LazyList.map (add pos) positionModifiers

        values =
            LazyList.map (\pos -> Dict.get pos map) positions

        newValue =
            LazyList.filterMap identity values |> LazyList.sum
    in
        ( newValue, Dict.insert pos newValue map )


additionValues : LazyList Int
additionValues =
    scanl (\pos ( _, map ) -> modifyMap pos map) ( 0, Dict.singleton ( 0, 0 ) 1 ) positions
        |> LazyList.map Tuple.first
